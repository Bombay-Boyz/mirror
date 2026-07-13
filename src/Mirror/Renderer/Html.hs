{-# LANGUAGE DataKinds #-}

-- | Rendering a validated 'Document' to an HTML page.
--
-- 'Block' and 'Inline' (§3) are recursive types — a block quote holds
-- blocks, a list item holds blocks, emphasis holds inline content that
-- can itself hold emphasis — so nothing in Haskell's type system stops
-- a document (adversarially constructed, or just a deeply nested JSON
-- payload; see §8) from nesting deep enough to exhaust the stack while
-- rendering. The algorithm threads an explicit depth counter through
-- every recursive descent and fails with a structured
-- 'Mirror.Error.UnrenderableNestingDepth' past a fixed bound, rather
-- than letting the underlying recursion answer the question by
-- crashing. That is why this module returns 'Either MirrorError Text'
-- rather than a bare 'Text': rendering a well-formed but pathological
-- document is a real, anticipated failure mode, not an exceptional one.
module Mirror.Renderer.Html (renderDocument) where

import Data.List.NonEmpty (toList)
import Data.Text (Text)

import Mirror.Document
import Mirror.Error
import Mirror.Html

-- | Chosen well above any depth a human-authored document would
-- plausibly reach, while stopping well short of exhausting the
-- default GHC RTS stack on adversarial or generated input.
maxNestingDepth :: Int
maxNestingDepth = 64

checkDepth :: Int -> Either MirrorError ()
checkDepth depth
  | depth > maxNestingDepth = Left (ErrRender (UnrenderableNestingDepth depth))
  | otherwise               = Right ()

-- | Takes the resolved stylesheet href explicitly rather than
-- hardcoding one, so "Mirror.Pipeline" controls whether the emitted
-- page links the embedded default or a user-supplied file.
renderDocument :: Text -> Document -> Either MirrorError Text
renderDocument cssHref doc = do
  bodyBlocks <- traverse (blockToHtml 0) (toList (documentBlocks doc))
  pure ("<!DOCTYPE html>\n" <> renderHtml (page bodyBlocks))
  where
    page bodyBlocks = tag "html" []
      [ tag "head" [] [titleTag, stylesheetLink]
      , tag "body" [] bodyBlocks
      ]
    titleTag = tag "title" []
      [ maybe (escape "Untitled") (escape . unNonEmptyText) (documentTitle doc) ]
    stylesheetLink = selfClosingTag "link"
      [ attr "rel" "stylesheet", attr "href" cssHref ]

-- | Exhaustive over every constructor of 'Block', checked by
-- @-Wincomplete-patterns -Werror@ — a property of the closed
-- constructor set, independent of the depth check, which guards
-- against a different failure mode entirely.
blockToHtml :: Int -> Block -> Either MirrorError (Html 'Escaped)
blockToHtml depth block = do
  checkDepth depth
  case block of
    Heading level inlines ->
      tag (headingTagName level) [] <$> inlinesToHtml (depth + 1) inlines
    Paragraph inlines ->
      tag "p" [] <$> inlinesToHtml (depth + 1) inlines
    BulletList kind items ->
      tag (listTagName kind) [] <$> traverse (listItemToHtml (depth + 1)) (toList items)
    TableBlock table ->
      tableToHtml (depth + 1) table
    BlockQuote blocks ->
      tag "blockquote" [] <$> traverse (blockToHtml (depth + 1)) (toList blocks)
    CodeBlock lang code ->
      pure (tag "pre" [] [ tag "code" (langClass lang) [ escape code ] ])
    Image src alt ->
      pure (selfClosingTag "img" [ attr "src" (unUrl src), attr "alt" alt ])
    HorizontalRule ->
      pure (selfClosingTag "hr" [])

headingTagName :: HeadingLevel -> Text
headingTagName = \case
  H1 -> "h1"; H2 -> "h2"; H3 -> "h3"; H4 -> "h4"; H5 -> "h5"; H6 -> "h6"

listTagName :: ListKind -> Text
listTagName = \case Ordered -> "ol"; Unordered -> "ul"

inlinesToHtml :: Int -> [Inline] -> Either MirrorError [Html 'Escaped]
inlinesToHtml depth inlines = do
  checkDepth depth
  traverse (inlineToHtml depth) inlines

-- | Exhaustive over every constructor of 'Inline', for the same reason
-- 'blockToHtml' is exhaustive over 'Block'.
inlineToHtml :: Int -> Inline -> Either MirrorError (Html 'Escaped)
inlineToHtml depth inl = case inl of
  Text t       -> pure (escape t)
  Emphasis xs  -> tag "em" [] <$> inlinesToHtml (depth + 1) xs
  Strong xs    -> tag "strong" [] <$> inlinesToHtml (depth + 1) xs
  Link url xs  -> tag "a" [attr "href" (unUrl url)] <$> inlinesToHtml (depth + 1) xs
  InlineCode t -> pure (tag "code" [] [ escape t ])
  SoftBreak    -> pure (selfClosingTag "br" [])

listItemToHtml :: Int -> ListItem -> Either MirrorError (Html 'Escaped)
listItemToHtml depth (ListItem blocks) = do
  checkDepth depth
  tag "li" [] <$> traverse (blockToHtml (depth + 1)) blocks

-- | Header row (if any) renders inside 'thead' as 'th' cells; body
-- rows render inside 'tbody' as 'td' cells.
tableToHtml :: Int -> Table -> Either MirrorError (Html 'Escaped)
tableToHtml depth table = do
  checkDepth depth
  headHtml <- traverse (rowToHtml "th") (tableHeader table)
  bodyHtml <- tag "tbody" [] <$> traverse (rowToHtml "td") (toList (tableBody table))
  pure (tag "table" [] (maybe [] (\h -> [tag "thead" [] [h]]) headHtml <> [bodyHtml]))
  where
    rowToHtml cellTag (TableRow cells) =
      tag "tr" [] <$> traverse (cellToHtml cellTag) (toList cells)
    cellToHtml cellTag cellInlines =
      tag cellTag [] <$> inlinesToHtml (depth + 1) cellInlines

langClass :: Maybe LanguageTag -> [(Text, Text)]
langClass = maybe [] (\l -> [attr "class" ("language-" <> languageSlug l)])

languageSlug :: LanguageTag -> Text
languageSlug = \case
  LangHaskell -> "haskell"; LangJavaScript -> "javascript"; LangPython -> "python"
  LangJson    -> "json";    LangShell      -> "shell";      LangSql    -> "sql"
  LangYaml    -> "yaml";    LangMarkdown   -> "markdown";   LangText   -> "text"
