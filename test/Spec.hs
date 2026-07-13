module Main (main) where

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (..), (==>))

import Mirror.Document
import Mirror.Error
import Mirror.Html
import Mirror.Parser.Docx (parseDocx)
import Mirror.Parser.Json (parseJson)
import Mirror.Parser.Markdown (parseMarkdown)

main :: IO ()
main = hspec $ do
  documentSpec
  htmlSpec
  markdownSpec
  docxSpec
  jsonSpec

--------------------------------------------------------------------------
-- Mirror.Document: the smart constructors every parser relies on
--------------------------------------------------------------------------

documentSpec :: Spec
documentSpec = describe "Mirror.Document smart constructors" $ do
  it "rejects a document with zero blocks" $
    mkDocument Nothing [] `shouldBe` Left (ErrValidation EmptyDocumentBody)

  it "accepts a document containing only an empty paragraph" $
    mkDocument Nothing [Paragraph []] `shouldSatisfy` isRight

  it "distinguishes zero rows from a row with zero cells" $ do
    mkTable Nothing []   `shouldBe` Left (ErrValidation EmptyTable)
    mkTable Nothing [[]] `shouldBe` Left (ErrValidation EmptyTableRow)

  it "reports a ragged table with the canonical and offending widths" $
    mkTable Nothing [[[Text "a"]], [[Text "b"], [Text "c"]]]
      `shouldBe` Left (ErrValidation (MismatchedTableColumns 1 2))

  it "accepts a rectangular table and reports its column count" $
    case mkTable (Just [[Text "H"]]) [[[Text "a"]]] of
      Right (TableBlock t) -> tableColumnCount t `shouldBe` 1
      other                -> expectationFailure ("expected a table, got " <> show other)

  it "rejects a syntactically invalid URL" $
    mkUrl "not a url with raw spaces" `shouldSatisfy` isInvalidUrl

  it "maps exactly the range 1..6 to a HeadingLevel" $ do
    map headingLevelFromInt [1 .. 6] `shouldBe` map Just [H1, H2, H3, H4, H5, H6]
    headingLevelFromInt 0 `shouldBe` Nothing
    headingLevelFromInt 7 `shouldBe` Nothing

  prop "any positive number of equal-width rows forms a valid table of that width" $
    \(Positive width) (Positive rowCount) ->
      let row = replicate width [Text "x"]
      in case mkTable Nothing (replicate rowCount row) of
           Right (TableBlock t) -> tableColumnCount t == width
           _                    -> False

  prop "two rows of differing widths are always rejected as ragged, never accepted" $
    \(Positive w1) (Positive w2) ->
      w1 /= w2 ==>
        case mkTable Nothing [replicate w1 [Text "x"], replicate w2 [Text "x"]] of
          Left (ErrValidation (MismatchedTableColumns _ _)) -> True
          _                                                 -> False

isInvalidUrl :: Either MirrorError a -> Bool
isInvalidUrl (Left (ErrValidation (InvalidUrl _))) = True
isInvalidUrl _                                     = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

--------------------------------------------------------------------------
-- Mirror.Html: escaping is the one thing that must never regress
--------------------------------------------------------------------------

htmlSpec :: Spec
htmlSpec = describe "Mirror.Html escaping" $ do
  it "escapes all five reserved characters in element content" $
    renderHtml (escape "<a>&'\"") `shouldBe` "&lt;a&gt;&amp;&#39;&quot;"

  it "escapes text used as an attribute value the same way" $
    renderHtml (tag "a" [attr "href" "\"><script>"] [escape "x"])
      `shouldBe` "<a href=\"&quot;&gt;&lt;script&gt;\">x</a>"

  prop "never leaves an unescaped <, >, \" or ' in the output" $ \s ->
    Text.all (`notElem` ("<>\"'" :: String)) (renderHtml (escape (Text.pack s)))

--------------------------------------------------------------------------
-- Mirror.Parser.Markdown
--------------------------------------------------------------------------

markdownSpec :: Spec
markdownSpec = describe "Mirror.Parser.Markdown" $ do
  it "REGRESSION: joins a wrapped paragraph's lines with SoftBreak, not one <p> per line" $
    parseMarkdown "This is a paragraph\nthat spans two lines."
      `shouldBe` mkDocument Nothing
        [Paragraph [Text "This is a paragraph", SoftBreak, Text "that spans two lines."]]

  it "keeps a blank-line-separated paragraph as its own block" $
    parseMarkdown "First.\n\nSecond."
      `shouldBe` mkDocument Nothing [Paragraph [Text "First."], Paragraph [Text "Second."]]

  it "recognises heading levels 1 and 6" $
    parseMarkdown "# One\n\n###### Six"
      `shouldBe` mkDocument Nothing [Heading H1 [Text "One"], Heading H6 [Text "Six"]]

  it "treats seven or more hashes as an ordinary paragraph, not a rejected document" $
    parseMarkdown "####### not a heading"
      `shouldBe` mkDocument Nothing [Paragraph [Text "####### not a heading"]]

  it "collapses multiple consecutive blank lines to a single separator" $
    parseMarkdown "A.\n\n\n\nB." `shouldBe` mkDocument Nothing [Paragraph [Text "A."], Paragraph [Text "B."]]

  it "rejects an all-blank document" $
    parseMarkdown "   \n\n  " `shouldBe` Left (ErrValidation EmptyDocumentBody)

  it "a heading immediately followed by a paragraph, with no blank line, still splits into two blocks" $
    parseMarkdown "# Title\nBody text right after"
      `shouldBe` mkDocument Nothing [Heading H1 [Text "Title"], Paragraph [Text "Body text right after"]]

  it "normalises CRLF line endings before joining or splitting" $
    parseMarkdown "Line one\r\nLine two\r\n\r\nSecond para\r\n"
      `shouldBe` mkDocument Nothing
        [ Paragraph [Text "Line one", SoftBreak, Text "Line two"]
        , Paragraph [Text "Second para"]
        ]

  -- Inline syntax (the Megaparsec inline layer, §6)
  it "parses **strong** emphasis" $
    parseMarkdown "**bold**" `shouldBe` mkDocument Nothing [Paragraph [Strong [Text "bold"]]]

  it "parses *emphasis*" $
    parseMarkdown "*italic*" `shouldBe` mkDocument Nothing [Paragraph [Emphasis [Text "italic"]]]

  it "parses a `code span`" $
    parseMarkdown "`x = 1`" `shouldBe` mkDocument Nothing [Paragraph [InlineCode "x = 1"]]

  it "parses a [text](url) link, recursing on the link text" $
    parseMarkdown "[**A**](https://anthropic.com)"
      `shouldBe` mkDocument Nothing
        [Paragraph [Link (url "https://anthropic.com") [Strong [Text "A"]]]]

  it "TRAP: an inner * must not steal from an outer ** closing marker" $
    parseMarkdown "**text*more**"
      `shouldBe` mkDocument Nothing [Paragraph [Strong [Text "text*more"]]]

  it "shields a code span's contents from emphasis interpretation" $
    parseMarkdown "`a*b*c`" `shouldBe` mkDocument Nothing [Paragraph [InlineCode "a*b*c"]]

  it "treats an escaped \\* as a literal asterisk, not a delimiter" $
    parseMarkdown "a\\*b" `shouldBe` mkDocument Nothing [Paragraph [Text "a*b"]]

  it "reports InvalidUrl for a link whose URL is not a valid URI" $
    parseMarkdown "[bad](not a url)" `shouldSatisfy` isInvalidUrl

  -- Block syntax (§6)
  it "groups consecutive - lines into one unordered list" $
    parseMarkdown "- one\n- two"
      `shouldBe` mkDocument Nothing
        [ BulletList Unordered
            ( ListItem [Paragraph [Text "one"]]
                :| [ListItem [Paragraph [Text "two"]]] )
        ]

  it "recognises an ordered list from N. markers" $
    parseMarkdown "1. first\n2. second"
      `shouldBe` mkDocument Nothing
        [ BulletList Ordered
            ( ListItem [Paragraph [Text "first"]]
                :| [ListItem [Paragraph [Text "second"]]] )
        ]

  it "unwraps a > blockquote and parses its contents recursively" $
    parseMarkdown "> quoted **bold**"
      `shouldBe` mkDocument Nothing
        [BlockQuote (Paragraph [Text "quoted ", Strong [Text "bold"]] :| [])]

  it "parses a fenced code block, preserving its body verbatim" $
    parseMarkdown "```haskell\nf x = x\n```"
      `shouldBe` mkDocument Nothing [CodeBlock (Just (languageTag "haskell")) "f x = x"]

  it "parses a fenced code block with no language tag" $
    parseMarkdown "```\nplain\n```"
      `shouldBe` mkDocument Nothing [CodeBlock Nothing "plain"]

  it "parses a pipe table with a delimiter row into a header + body" $
    parseMarkdown "| A | B |\n| - | - |\n| 1 | 2 |"
      `shouldBe` mkDocument Nothing
        [ tableOrError
            (Just [[Text "A"], [Text "B"]])
            [[[Text "1"], [Text "2"]]]
        ]

  it "reports MismatchedTableColumns for a ragged pipe table -- the shared validation" $
    parseMarkdown "| A | B |\n| - | - |\n| 1 |"
      `shouldBe` Left (ErrValidation (MismatchedTableColumns 2 1))

  it "recognises a --- thematic break as a horizontal rule" $
    parseMarkdown "above\n\n---\n\nbelow"
      `shouldBe` mkDocument Nothing
        [Paragraph [Text "above"], HorizontalRule, Paragraph [Text "below"]]

  it "recognises a standalone image line" $
    parseMarkdown "![a cat](https://ex.com/c.png)"
      `shouldBe` mkDocument Nothing [Image (url "https://ex.com/c.png") "a cat"]

--------------------------------------------------------------------------
-- Mirror.Parser.Docx
--------------------------------------------------------------------------

docxSpec :: Spec
docxSpec = describe "Mirror.Parser.Docx" $ do
  it "REGRESSION: resolves a Heading1-styled paragraph's level from w:pStyle/@w:val" $ do
    result <- runExceptT (parseDocx "t.docx" (docxBytes
      "<w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr><w:r><w:t>Title</w:t></w:r></w:p>"))
    result `shouldBe` mkDocument Nothing [Heading H1 [Text "Title"]]

  it "REGRESSION: an empty paragraph renders as an empty block, not a rejected document" $ do
    result <- runExceptT (parseDocx "t.docx" (docxBytes
      "<w:p><w:r><w:t>Before</w:t></w:r></w:p><w:p/><w:p><w:r><w:t>After</w:t></w:r></w:p>"))
    result `shouldBe` mkDocument Nothing
      [Paragraph [Text "Before"], Paragraph [], Paragraph [Text "After"]]

  it "honours an explicit w:val=\"0\" as turning bold off despite w:b being present" $ do
    result <- runExceptT (parseDocx "t.docx" (docxBytes
      "<w:p><w:r><w:rPr><w:b w:val=\"0\"/></w:rPr><w:t>plain</w:t></w:r></w:p>"))
    result `shouldBe` mkDocument Nothing [Paragraph [Text "plain"]]

  -- Hyperlinks (r:id resolved through document.xml.rels)
  it "resolves a w:hyperlink through the relationships part into a Link" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich
      [("word/_rels/document.xml.rels", relsXml [("rId1", "https://anthropic.com")])]
      "<w:p><w:hyperlink r:id=\"rId1\"><w:r><w:t>click</w:t></w:r></w:hyperlink></w:p>"))
    result `shouldBe` mkDocument Nothing
      [Paragraph [Link (url "https://anthropic.com") [Text "click"]]]

  it "reports DocxUnresolvedRelationship for a hyperlink whose r:id is not in the rels" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich []
      "<w:p><w:hyperlink r:id=\"rId9\"><w:r><w:t>x</w:t></w:r></w:hyperlink></w:p>"))
    result `shouldBe` Left (ErrParse (DocxUnresolvedRelationship "rId9"))

  it "reports InvalidUrl for a hyperlink whose resolved target is not a valid URI" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich
      [("word/_rels/document.xml.rels", relsXml [("rId1", "not a url")])]
      "<w:p><w:hyperlink r:id=\"rId1\"><w:r><w:t>x</w:t></w:r></w:hyperlink></w:p>"))
    result `shouldSatisfy` isInvalidUrl

  -- Line breaks
  it "turns a w:br inside a run into a SoftBreak" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich []
      "<w:p><w:r><w:t>a</w:t><w:br/><w:t>b</w:t></w:r></w:p>"))
    result `shouldBe` mkDocument Nothing [Paragraph [Text "a", SoftBreak, Text "b"]]

  -- Horizontal rule (bottom paragraph border, Word's --- autoformat)
  it "recognises a paragraph bottom border as a HorizontalRule" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich []
      "<w:p><w:pPr><w:pBdr><w:bottom w:val=\"single\"/></w:pBdr></w:pPr></w:p>"))
    result `shouldBe` mkDocument Nothing [HorizontalRule]

  -- Block quotes (Word's built-in Quote style, grouped)
  it "groups consecutive Quote-styled paragraphs into one BlockQuote" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich []
      (  "<w:p><w:pPr><w:pStyle w:val=\"Quote\"/></w:pPr><w:r><w:t>one</w:t></w:r></w:p>"
      <> "<w:p><w:pPr><w:pStyle w:val=\"Quote\"/></w:pPr><w:r><w:t>two</w:t></w:r></w:p>")))
    result `shouldBe` mkDocument Nothing
      [BlockQuote (Paragraph [Text "one"] :| [Paragraph [Text "two"]])]

  -- Tables (w:tbl / w:tr / w:tc), never a header row
  it "parses a w:tbl into a header-less TableBlock" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich []
      (  "<w:tbl><w:tr>"
      <> "<w:tc><w:p><w:r><w:t>A</w:t></w:r></w:p></w:tc>"
      <> "<w:tc><w:p><w:r><w:t>B</w:t></w:r></w:p></w:tc>"
      <> "</w:tr></w:tbl>")))
    result `shouldBe` mkDocument Nothing
      [tableOrError Nothing [[[Text "A"], [Text "B"]]]]

  it "reports MismatchedTableColumns for a ragged w:tbl -- the shared validation" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich []
      (  "<w:tbl>"
      <> "<w:tr><w:tc><w:p><w:r><w:t>A</w:t></w:r></w:p></w:tc>"
      <>        "<w:tc><w:p><w:r><w:t>B</w:t></w:r></w:p></w:tc></w:tr>"
      <> "<w:tr><w:tc><w:p><w:r><w:t>C</w:t></w:r></w:p></w:tc></w:tr>"
      <> "</w:tbl>")))
    result `shouldBe` Left (ErrValidation (MismatchedTableColumns 2 1))

  -- Lists (numId resolved through numbering.xml, flat)
  it "groups consecutive bullet-numbered paragraphs into an unordered list" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich
      [("word/numbering.xml", numberingXml "bullet")]
      (  numberedPara "1" "one"
      <> numberedPara "1" "two")))
    result `shouldBe` mkDocument Nothing
      [ BulletList Unordered
          ( ListItem [Paragraph [Text "one"]]
              :| [ListItem [Paragraph [Text "two"]]] )
      ]

  it "resolves a decimal numFmt to an ordered list" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich
      [("word/numbering.xml", numberingXml "decimal")]
      (numberedPara "1" "only")))
    result `shouldBe` mkDocument Nothing
      [BulletList Ordered (ListItem [Paragraph [Text "only"]] :| [])]

  it "reports DocxUnresolvedNumbering when a numId has no numbering.xml entry" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich []
      (numberedPara "7" "orphan")))
    result `shouldBe` Left (ErrParse (DocxUnresolvedNumbering "7"))

  -- Images (a:blip r:embed resolved to media bytes, base64 data: URI)
  it "embeds a drawing's image as a base64 data: URI Image block" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich
      [ ("word/_rels/document.xml.rels", relsXml [("rId5", "media/image1.png")])
      , ("word/media/image1.png", "hello")
      ]
      (drawingPara "rId5" "a cat")))
    result `shouldBe` mkDocument Nothing
      [Image (url "data:image/png;base64,aGVsbG8=") "a cat"]

  it "reports DocxUnsupportedImageFormat for an unrecognised media extension" $ do
    result <- runExceptT (parseDocx "t.docx" (docxRich
      [ ("word/_rels/document.xml.rels", relsXml [("rId5", "media/image1.tiff")])
      , ("word/media/image1.tiff", "hello")
      ]
      (drawingPara "rId5" "x")))
    result `shouldBe` Left (ErrParse (DocxUnsupportedImageFormat "tiff"))

--------------------------------------------------------------------------
-- DOCX test builders
--------------------------------------------------------------------------

-- | The original minimal builder: document.xml only, w: namespace only.
docxBytes :: String -> BSLC.ByteString
docxBytes paragraphsXml = docxRich [] paragraphsXml

-- | A builder that adds arbitrary extra parts (rels, numbering, media)
-- alongside a document.xml declaring all four namespaces the extended
-- parser needs (w, r, a, wp).
docxRich :: [(String, String)] -> String -> BSLC.ByteString
docxRich extraParts body =
  fromArchive (foldr addEntryToArchive emptyArchive (docEntry : extraEntries))
  where
    docEntry     = toEntry "word/document.xml" 0 (BSLC.pack (wrap body))
    extraEntries = [toEntry p 0 (BSLC.pack c) | (p, c) <- extraParts]
    wrap b =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?><w:document \
      \xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" \
      \xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" \
      \xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" \
      \xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\">\
      \<w:body>" <> b <> "</w:body></w:document>"

relsXml :: [(String, String)] -> String
relsXml pairs =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
    <> concat [ "<Relationship Id=\"" <> i <> "\" Target=\"" <> t <> "\"/>" | (i, t) <- pairs ]
    <> "</Relationships>"

-- | A numbering.xml mapping numId 1 -> abstractNumId 0, whose level-0
-- format is the given numFmt val ("bullet" or e.g. "decimal").
numberingXml :: String -> String
numberingXml fmt =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<w:numbering xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">\
  \<w:abstractNum w:abstractNumId=\"0\"><w:lvl w:ilvl=\"0\"><w:numFmt w:val=\""
    <> fmt <> "\"/></w:lvl></w:abstractNum>\
  \<w:num w:numId=\"1\"><w:abstractNumId w:val=\"0\"/></w:num></w:numbering>"

numberedPara :: String -> String -> String
numberedPara numId txt =
  "<w:p><w:pPr><w:numPr><w:numId w:val=\"" <> numId <> "\"/></w:numPr></w:pPr>\
  \<w:r><w:t>" <> txt <> "</w:t></w:r></w:p>"

drawingPara :: String -> String -> String
drawingPara embedId alt =
  "<w:p><w:r><w:drawing><wp:inline><wp:docPr descr=\"" <> alt <> "\"/>\
  \<a:blip r:embed=\"" <> embedId <> "\"/></wp:inline></w:drawing></w:r></w:p>"

url :: Text.Text -> Url
url t = either (error . show) id (mkUrl t)

languageTag :: Text.Text -> LanguageTag
languageTag t = maybe (error ("bad language tag: " <> show t)) id (languageTagFromText t)

tableOrError :: Maybe [[Inline]] -> [[[Inline]]] -> Block
tableOrError header rows = either (error . show) id (mkTable header rows)

--------------------------------------------------------------------------
-- Mirror.Parser.Json
--------------------------------------------------------------------------

jsonSpec :: Spec
jsonSpec = describe "Mirror.Parser.Json" $ do
  it "round-trips a heading" $
    parseJson (BSLC.pack
      "{\"blocks\":[{\"type\":\"heading\",\"level\":2,\"content\":[{\"type\":\"text\",\"text\":\"Hi\"}]}]}")
      `shouldBe` mkDocument Nothing [Heading H2 [Text "Hi"]]

  it "reports MismatchedTableColumns for a ragged table -- the same error any source gets" $
    parseJson (BSLC.pack
      ("{\"blocks\":[{\"type\":\"table\",\"rows\":["
        <> "[[{\"type\":\"text\",\"text\":\"a\"}]],"
        <> "[[{\"type\":\"text\",\"text\":\"b\"}],[{\"type\":\"text\",\"text\":\"c\"}]]"
        <> "]}]}"))
      `shouldBe` Left (ErrValidation (MismatchedTableColumns 1 2))

  it "reports JsonSchemaViolation for an unrecognised block type" $
    parseJson (BSLC.pack "{\"blocks\":[{\"type\":\"bogus\"}]}") `shouldSatisfy` isSchemaViolation

  it "reports JsonSchemaViolation for a heading level outside 1..6" $
    parseJson (BSLC.pack "{\"blocks\":[{\"type\":\"heading\",\"level\":9,\"content\":[]}]}")
      `shouldSatisfy` isSchemaViolation

  it "reports JsonSyntax for input that isn't well-formed JSON at all" $
    parseJson (BSLC.pack "{not json") `shouldSatisfy` isJsonSyntaxError

  it "rejects an empty blocks array exactly as mkDocument would for any source" $
    parseJson (BSLC.pack "{\"blocks\":[]}") `shouldBe` Left (ErrValidation EmptyDocumentBody)

  it "rejects a list with zero items" $
    parseJson (BSLC.pack "{\"blocks\":[{\"type\":\"list\",\"kind\":\"unordered\",\"items\":[]}]}")
      `shouldBe` Left (ErrValidation EmptyList)

  it "rejects a blockquote with zero blocks" $
    parseJson (BSLC.pack "{\"blocks\":[{\"type\":\"blockquote\",\"content\":[]}]}")
      `shouldBe` Left (ErrValidation EmptyBlockQuote)

  it "reports JsonSchemaViolation for a table missing the required \"rows\" key" $
    parseJson (BSLC.pack "{\"blocks\":[{\"type\":\"table\"}]}") `shouldSatisfy` isSchemaViolation

  it "reports JsonSchemaViolation for an unrecognised list \"kind\"" $
    parseJson (BSLC.pack "{\"blocks\":[{\"type\":\"list\",\"kind\":\"sideways\",\"items\":[[]]}]}")
      `shouldSatisfy` isSchemaViolation

  it "rejects an unrecognised code_block \"language\"" $
    parseJson (BSLC.pack "{\"blocks\":[{\"type\":\"code_block\",\"language\":\"cobol\",\"code\":\"x\"}]}")
      `shouldSatisfy` isInvalidLanguageTag

  it "rejects a syntactically invalid link href, same as any other URL" $
    parseJson (BSLC.pack
      "{\"blocks\":[{\"type\":\"paragraph\",\"content\":[{\"type\":\"link\",\"href\":\"not a url\",\"content\":[]}]}]}")
      `shouldSatisfy` isInvalidUrl

isInvalidLanguageTag :: Either MirrorError a -> Bool
isInvalidLanguageTag (Left (ErrValidation (InvalidLanguageTag _))) = True
isInvalidLanguageTag _                                            = False

isSchemaViolation :: Either MirrorError a -> Bool
isSchemaViolation (Left (ErrParse (JsonSchemaViolation _))) = True
isSchemaViolation _                                         = False

isJsonSyntaxError :: Either MirrorError a -> Bool
isJsonSyntaxError (Left (ErrParse (JsonSyntax _))) = True
isJsonSyntaxError _                                 = False
