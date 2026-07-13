-- | DOCX extraction and parsing.
--
-- A @.docx@ is a zip archive whose main content lives in
-- @word\/document.xml@, but three features below need one of two
-- further parts, each read only if a construct that needs it is
-- actually encountered: @word\/_rels\/document.xml.rels@ (resolving a
-- hyperlink's or image's relationship id to its target) and
-- @word\/numbering.xml@ (resolving a list paragraph's @numId@ to
-- "ordered" or "unordered"). Both are genuinely separate OOXML
-- subsystems from the paragraph content itself, which is why they get
-- their own, distinctly-named errors (§2) rather than being folded
-- into 'DocxMalformedXml'. Neither is an error to be /missing/ — a
-- document with no hyperlinks or lists simply has no need of them —
-- only to be referenced and unresolvable.
--
-- The algorithm for the body as a whole: @w:body@ is a flat sequence
-- of paragraph (@w:p@) and table (@w:tbl@) elements, scanned once,
-- left to right — the same classify-and-group shape §6 uses for
-- Markdown, just over XML cursors instead of text lines. A table is
-- always its own block; a run of consecutive paragraphs sharing the
-- same list @numId@ groups into one list, the same way Markdown groups
-- consecutive marker lines; a run of consecutive paragraphs styled
-- @Quote@ or @IntenseQuote@ (Word's own built-in quote styles) groups
-- into one block quote; anything else is a heading (by style), a
-- horizontal rule (by paragraph border), or an ordinary paragraph.
--
-- Per-paragraph, per-run conversion now produces
-- "Mirror.Document.Raw"'s staging types rather than final 'Mirror.Document.Block'\/'Mirror.Document.Inline'
-- values directly, because hyperlinks, images, and tables are
-- genuinely fallible (a hyperlink's URL must be syntactically valid; a
-- table must be rectangular) in a way plain paragraphs and bold\/italic
-- runs never were — the same two-phase discipline §8 established for
-- JSON.
--
-- Scope, each a deliberate simplification stated plainly rather than
-- silently unsupported:
--
--   * Lists are flat — see §6's identical simplification and the same
--     reasoning.
--   * Only Word's own default heading and quote style IDs are
--     recognised (@Heading1@..@Heading6@, @Quote@, @IntenseQuote@); a
--     renamed or custom style requires resolving @word\/styles.xml@'s
--     inheritance chain, out of scope here (§13).
--   * There is no reliable native DOCX signal for "this is code" the
--     way Markdown has fences — no heuristic (font, style name) is
--     applied, and DOCX code blocks are not supported.
--   * A table's first row is never treated as a header — DOCX has no
--     equivalent of a Markdown table's delimiter row to signal one.
--   * Images are embedded as base64 @data:@ URIs directly in the
--     rendered HTML (§9), keeping the page self-contained, rather
--     than written out as sibling files.
module Mirror.Parser.Docx (parseDocx) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.Trans.Except (ExceptT (..))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BSL
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry, toArchive)
import qualified Text.XML as XML
import Text.XML (Node (NodeElement), Name (..), def, elementName, parseLBS)
import Text.XML.Cursor
  (Cursor, attribute, checkName, content, element, fromDocument, node, ($/), ($//), (&/))

import Mirror.Document (Document, mkDocument)
import Mirror.Document.Raw
import Mirror.Error

--------------------------------------------------------------------------
-- Namespaces. Four distinct ones are in play — a real and easy-to-miss
-- source of mistakes: wordprocessingml for the document content
-- itself, package-relationships for the .rels file's own elements,
-- officeDocument-relationships for the r:id / r:embed *attributes*
-- that appear inside document.xml and point into the .rels file, and
-- drawingml for the image-drawing elements a run can contain.
--------------------------------------------------------------------------

wordNs :: Text -> Name
wordNs local = Name local (Just "http://schemas.openxmlformats.org/wordprocessingml/2006/main") Nothing

packageRelNs :: Text -> Name
packageRelNs local = Name local (Just "http://schemas.openxmlformats.org/package/2006/relationships") Nothing

officeRelNs :: Text -> Name
officeRelNs local = Name local (Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships") Nothing

drawingMainNs :: Text -> Name
drawingMainNs local = Name local (Just "http://schemas.openxmlformats.org/drawingml/2006/main") Nothing

wordDrawingNs :: Text -> Name
wordDrawingNs local = Name local (Just "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing") Nothing

-- | An unprefixed attribute (@Id@, @Target@, @descr@) is never in any
-- namespace, even inside an element that has one — a default
-- namespace applies to element names, never to unprefixed attributes.
unqualifiedAttr :: Text -> Cursor -> [Text]
unqualifiedAttr local = attribute (Name local Nothing Nothing)

tshow :: Show a => a -> Text
tshow = Text.pack . show

--------------------------------------------------------------------------
-- Entry point, zip and XML plumbing
--------------------------------------------------------------------------

-- | The one place, alongside "Mirror.Pipeline", that performs 'IO'.
-- Granted narrowly (§0, rule 7): @zip-archive@'s functions are typed
-- pure but can raise an exception the moment their lazily-produced
-- 'ByteString' is consumed. Every zip-reading step — the three known
-- part names, and every embedded image an 'ExceptT MirrorError IO'
-- pass over the whole document turns up — goes through 'loadPart',
-- the one place that force happens, guarded. Everything downstream of
-- that (relationships, numbering, body structure, images-as-data-URIs)
-- is pure, taking already-extracted bytes or an already-built lookup
-- table as a plain argument.
parseDocx :: FilePath -> BSL.ByteString -> ExceptT MirrorError IO Document
parseDocx path raw = do
  let archive = toArchive raw
  docBytes    <- requirePart path archive "word/document.xml"
  docCursor   <- ExceptT (pure (fromDocument <$> parseXmlPart "word/document.xml" docBytes))
  relsBytes   <- loadPart path archive "word/_rels/document.xml.rels"
  numBytes    <- loadPart path archive "word/numbering.xml"
  rels        <- ExceptT (pure (maybe (Right Map.empty) parseRelationships relsBytes))
  numFmts     <- ExceptT (pure (maybe (Right Map.empty) parseNumberingFormats numBytes))
  imageData   <- loadImageDataUris path archive rels docCursor
  rawBlocks   <- ExceptT (pure (bodyBlocksFromCursors imageData rels numFmts (bodyChildCursors docCursor)))
  finalBlocks <- ExceptT (pure (traverse toBlock rawBlocks))
  ExceptT (pure (mkDocument Nothing finalBlocks))

-- | Extracts one zip entry's bytes if present, forcing full
-- evaluation (via 'BSL.length') at the point of extraction — exactly
-- where a corrupt archive's decompression failure would throw —
-- rather than leaving that force to happen lazily on some later,
-- unrelated line.
loadPart :: FilePath -> Archive -> Text -> ExceptT MirrorError IO (Maybe BSL.ByteString)
loadPart archivePath archive partName = ExceptT $ do
  outcome <- try (evaluate forced)
  pure $ case (outcome :: Either SomeException (Maybe BSL.ByteString)) of
    Left _        -> Left (ErrParse (DocxNotAZipArchive archivePath))
    Right maybeBs -> Right maybeBs
  where
    forced = case findEntryByPath (Text.unpack partName) archive of
      Nothing -> Nothing
      Just e  -> let bytes = fromEntry e in BSL.length bytes `seq` Just bytes

requirePart :: FilePath -> Archive -> Text -> ExceptT MirrorError IO BSL.ByteString
requirePart archivePath archive partName = do
  maybeBs <- loadPart archivePath archive partName
  ExceptT (pure (maybe (Left (ErrParse (DocxMissingPart partName))) Right maybeBs))

parseXmlPart :: Text -> BSL.ByteString -> Either MirrorError XML.Document
parseXmlPart partName bytes =
  either (Left . ErrParse . DocxMalformedXml partName . tshow) Right (parseLBS def bytes)

bodyChildCursors :: Cursor -> [Cursor]
bodyChildCursors docCursor =
  docCursor $// element (wordNs "body") &/ checkName isBodyChild
  where
    isBodyChild n = n == wordNs "p" || n == wordNs "tbl"

isNamed :: Text -> Cursor -> Bool
isNamed local c = case node c of
  NodeElement e -> elementName e == wordNs local
  _             -> False

--------------------------------------------------------------------------
-- Relationships (word/_rels/document.xml.rels): rId -> Target
--------------------------------------------------------------------------

parseRelationships :: BSL.ByteString -> Either MirrorError (Map Text Text)
parseRelationships bytes = do
  xmlDoc <- parseXmlPart "word/_rels/document.xml.rels" bytes
  let relCursors = fromDocument xmlDoc $// element (packageRelNs "Relationship")
  pure (Map.fromList (mapMaybe extractPair relCursors))
  where
    extractPair c = do
      rid    <- listToMaybe (unqualifiedAttr "Id" c)
      target <- listToMaybe (unqualifiedAttr "Target" c)
      pure (rid, target)

--------------------------------------------------------------------------
-- Numbering (word/numbering.xml): numId -> "ordered" | "unordered",
-- resolved numId -> abstractNumId -> (level-0 numFmt).
--------------------------------------------------------------------------

parseNumberingFormats :: BSL.ByteString -> Either MirrorError (Map Text Text)
parseNumberingFormats bytes = do
  xmlDoc <- parseXmlPart "word/numbering.xml" bytes
  let docCursor          = fromDocument xmlDoc
      abstractFormats    = Map.fromList (mapMaybe extractAbstractFormat (docCursor $// element (wordNs "abstractNum")))
      numToAbstract      = Map.fromList (mapMaybe extractNumMapping (docCursor $// element (wordNs "num")))
  pure (Map.mapMaybe (`Map.lookup` abstractFormats) numToAbstract)
  where
    extractAbstractFormat c = do
      abstractId   <- listToMaybe (attribute (wordNs "abstractNumId") c)
      let isLevelZero lc = listToMaybe (attribute (wordNs "ilvl") lc) == Just "0"
      lvl0         <- listToMaybe (filter isLevelZero (c $// element (wordNs "lvl")))
      numFmtCursor <- listToMaybe (lvl0 $// element (wordNs "numFmt"))
      fmtVal       <- listToMaybe (attribute (wordNs "val") numFmtCursor)
      pure (abstractId, if fmtVal == "bullet" then "unordered" else "ordered" :: Text)
    extractNumMapping c = do
      numId            <- listToMaybe (attribute (wordNs "numId") c)
      abstractIdCursor <- listToMaybe (c $// element (wordNs "abstractNumId"))
      abstractId       <- listToMaybe (attribute (wordNs "val") abstractIdCursor)
      pure (numId, abstractId)

--------------------------------------------------------------------------
-- Images: an upfront IO pass resolves every a:blip in the document to
-- a base64 data URI, so everything downstream can look one up as a
-- pure map access instead of touching the zip archive again.
--------------------------------------------------------------------------

loadImageDataUris :: FilePath -> Archive -> Map Text Text -> Cursor -> ExceptT MirrorError IO (Map Text Text)
loadImageDataUris archivePath archive rels docCursor = do
  let rids = nub (mapMaybe (listToMaybe . attribute (officeRelNs "embed")) (docCursor $// element (drawingMainNs "blip")))
  pairs <- traverse (\rid -> (,) rid <$> loadOneImageDataUri archivePath archive rels rid) rids
  pure (Map.fromList pairs)

loadOneImageDataUri :: FilePath -> Archive -> Map Text Text -> Text -> ExceptT MirrorError IO Text
loadOneImageDataUri archivePath archive rels rid = do
  target     <- ExceptT (pure (maybe (Left (ErrParse (DocxUnresolvedRelationship rid))) Right (Map.lookup rid rels)))
  mimeType   <- ExceptT (pure (mimeTypeFromExtension target))
  let partName = resolveMediaTarget target
  maybeBytes <- loadPart archivePath archive partName
  bytes      <- ExceptT (pure (maybe (Left (ErrParse (DocxMissingPart partName))) Right maybeBytes))
  let base64Text = TE.decodeUtf8 (Base64.encode (BSL.toStrict bytes))
  pure ("data:" <> mimeType <> ";base64," <> base64Text)

-- | A relationship target is conventionally relative to the
-- referencing part's own directory (@word\/@, for anything reached
-- from @document.xml@'s relationships) unless it starts with @\/@, in
-- which case it is already archive-root-relative.
resolveMediaTarget :: Text -> Text
resolveMediaTarget target = fromMaybe ("word/" <> target) (Text.stripPrefix "/" target)

-- | A closed, recognised set — matching every other closed-vocabulary
-- decision in this codebase (§0, rule 3). An unrecognised extension is
-- 'DocxUnsupportedImageFormat', not a silent guess or a dropped image.
mimeTypeFromExtension :: Text -> Either MirrorError Text
mimeTypeFromExtension partName =
  case Text.toLower (Text.takeWhileEnd (/= '.') partName) of
    "png"  -> Right "image/png"
    "jpg"  -> Right "image/jpeg"
    "jpeg" -> Right "image/jpeg"
    "gif"  -> Right "image/gif"
    "bmp"  -> Right "image/bmp"
    ext    -> Left (ErrParse (DocxUnsupportedImageFormat ext))

--------------------------------------------------------------------------
-- The body scan: groups w:tbl into tables, consecutive same-numId
-- paragraphs into one list, consecutive Quote/IntenseQuote paragraphs
-- into one block quote — the same classify-and-group shape §6 uses.
--------------------------------------------------------------------------

bodyBlocksFromCursors :: Map Text Text -> Map Text Text -> Map Text Text -> [Cursor] -> Either MirrorError [RawBlock]
bodyBlocksFromCursors imageData rels numFmts = go
  where
    go [] = Right []
    go (c : cs)
      | isNamed "tbl" c
      = (:) <$> tableToRawBlock rels c <*> go cs

      | Just numId <- paragraphNumId c
      = let (itemCursors, remaining) = span (\c' -> paragraphNumId c' == Just numId) (c : cs)
        in do
             kindText <- maybe (Left (ErrParse (DocxUnresolvedNumbering numId))) Right (Map.lookup numId numFmts)
             items    <- traverse (\ic -> (: []) <$> paragraphToRawBlock imageData rels ic) itemCursors
             (RawList kindText items :) <$> go remaining

      | isQuoteStyled c
      = let (quoteCursors, remaining) = span isQuoteStyled (c : cs)
        in do
             quoteBlocks <- traverse (paragraphToRawBlock imageData rels) quoteCursors
             (RawBlockQuote quoteBlocks :) <$> go remaining

      | otherwise
      = (:) <$> paragraphToRawBlock imageData rels c <*> go cs

--------------------------------------------------------------------------
-- Paragraph-level classification: style-based heading and quote
-- detection, paragraph-border horizontal rules, list membership.
--------------------------------------------------------------------------

wordHeadingStyleIds :: [(Text, Int)]
wordHeadingStyleIds =
  [ ("Heading1", 1), ("Heading2", 2), ("Heading3", 3)
  , ("Heading4", 4), ("Heading5", 5), ("Heading6", 6)
  ]

paragraphStyleId :: Cursor -> Maybe Text
paragraphStyleId p = do
  styleCursor <- listToMaybe (p $// element (wordNs "pStyle"))
  listToMaybe (attribute (wordNs "val") styleCursor)

headingLevelFromStyleInt :: Cursor -> Maybe Int
headingLevelFromStyleInt p = paragraphStyleId p >>= (`lookup` wordHeadingStyleIds)

-- | Word's own built-in quote styles — a custom or renamed style is
-- out of scope, the same simplification applied to headings.
isQuoteStyled :: Cursor -> Bool
isQuoteStyled c = isNamed "p" c && maybe False (`elem` ["Quote", "IntenseQuote"]) (paragraphStyleId c)

-- | The mechanism Word actually uses for the "type --- and press
-- enter" horizontal rule autoformat: a bottom paragraph border.
hasBottomBorder :: Cursor -> Bool
hasBottomBorder c =
  isNamed "p" c && not (null (c $// element (wordNs "pBdr") &/ element (wordNs "bottom")))

paragraphNumId :: Cursor -> Maybe Text
paragraphNumId c
  | not (isNamed "p" c) = Nothing
  | otherwise = do
      numPrCursor <- listToMaybe (c $// element (wordNs "numPr"))
      numIdCursor <- listToMaybe (numPrCursor $// element (wordNs "numId"))
      listToMaybe (attribute (wordNs "val") numIdCursor)

paragraphDrawing :: Cursor -> Maybe Cursor
paragraphDrawing p = listToMaybe (p $// element (wordNs "drawing"))

-- | A paragraph counts as "just an image" if it contains a drawing
-- and no other text content anywhere in it — the same judgment §6
-- applies to a standalone Markdown image line, and for the same
-- reason: 'Mirror.Document.Image' is block-level (§3), so there is no
-- way to represent a drawing that shares a paragraph with real prose.
-- A drawing found in a paragraph that fails this check is not
-- specially recognised — a stated, narrow limitation.
paragraphIsStandaloneDrawing :: Cursor -> Bool
paragraphIsStandaloneDrawing p = case paragraphDrawing p of
  Nothing -> False
  Just _  -> Text.null (Text.concat (p $// element (wordNs "t") &/ content))

paragraphToRawBlock :: Map Text Text -> Map Text Text -> Cursor -> Either MirrorError RawBlock
paragraphToRawBlock imageData rels p
  | hasBottomBorder p
  = Right RawHorizontalRule

  | paragraphIsStandaloneDrawing p
  , Just drawingCursor <- paragraphDrawing p
  = drawingToRawImage imageData drawingCursor

  | otherwise
  = do
      inlines <- paragraphRawInlines rels p
      pure $ case headingLevelFromStyleInt p of
        Just level -> RawHeading level inlines
        Nothing    -> RawParagraph inlines

drawingToRawImage :: Map Text Text -> Cursor -> Either MirrorError RawBlock
drawingToRawImage imageData drawingCursor = do
  blipCursor <- note1 "w:drawing has no a:blip" (listToMaybe (drawingCursor $// element (drawingMainNs "blip")))
  rid        <- note1 "a:blip missing r:embed" (listToMaybe (attribute (officeRelNs "embed") blipCursor))
  dataUri    <- maybe (Left (ErrParse (DocxUnresolvedRelationship rid))) Right (Map.lookup rid imageData)
  let docPrCursors = drawingCursor $// element (wordDrawingNs "docPr")
      altCandidates = concatMap (\c -> unqualifiedAttr "descr" c ++ unqualifiedAttr "title" c) docPrCursors
      altText = fromMaybe "" (listToMaybe altCandidates)
  pure (RawImage dataUri altText)
  where
    note1 msg = maybe (Left (ErrParse (DocxMalformedXml "word/document.xml" msg))) Right

--------------------------------------------------------------------------
-- Tables: w:tbl / w:tr / w:tc. Never treated as having a header row —
-- DOCX has no equivalent of a Markdown table's delimiter row to
-- signal one.
--------------------------------------------------------------------------

tableToRawBlock :: Map Text Text -> Cursor -> Either MirrorError RawBlock
tableToRawBlock rels tblCursor = do
  rows <- traverse (rowToRawRow rels) (tblCursor $/ element (wordNs "tr"))
  pure (RawTable Nothing rows)

rowToRawRow :: Map Text Text -> Cursor -> Either MirrorError RawRow
rowToRawRow rels rowCursor = traverse (cellToRawCell rels) (rowCursor $/ element (wordNs "tc"))

-- | A cell's paragraphs (a table cell may contain more than one) are
-- joined with a soft break: 'RawCell' is a single inline-content slot
-- ('[RawInline]'), not a sequence of blocks, the same representation
-- every other source's table cells use (§3, §8).
cellToRawCell :: Map Text Text -> Cursor -> Either MirrorError RawCell
cellToRawCell rels cellCursor = do
  inlineLists <- traverse (paragraphRawInlines rels) (cellCursor $/ element (wordNs "p"))
  pure (intercalateSoftBreak inlineLists)

intercalateSoftBreak :: [[RawInline]] -> [RawInline]
intercalateSoftBreak []       = []
intercalateSoftBreak [x]      = x
intercalateSoftBreak (x : xs) = x ++ [RawSoftBreak] ++ intercalateSoftBreak xs

--------------------------------------------------------------------------
-- Paragraph inline content: runs and hyperlinks, siblings within a
-- paragraph. A run's own text and any w:br within it are read
-- directly; a hyperlink additionally resolves its r:id and wraps its
-- own runs' inlines in a RawLink.
--------------------------------------------------------------------------

paragraphRawInlines :: Map Text Text -> Cursor -> Either MirrorError [RawInline]
paragraphRawInlines rels p = concat <$> traverse (childToRawInlines rels) (p $/ checkName isInlineChild)
  where
    isInlineChild n = n == wordNs "r" || n == wordNs "hyperlink"

childToRawInlines :: Map Text Text -> Cursor -> Either MirrorError [RawInline]
childToRawInlines rels c
  | isNamed "hyperlink" c
  = do
      rid        <- maybe (Left (ErrParse (DocxMalformedXml "word/document.xml" "w:hyperlink missing r:id"))) Right
                      (listToMaybe (attribute (officeRelNs "id") c))
      target     <- maybe (Left (ErrParse (DocxUnresolvedRelationship rid))) Right (Map.lookup rid rels)
      runInlines <- concat <$> traverse runToRawInlines (c $/ element (wordNs "r"))
      pure [RawLink target runInlines]

  | isNamed "r" c
  = runToRawInlines c

  | otherwise
  = pure []

-- | A run's text (@w:t@) and line breaks (@w:br@), in document order,
-- with bold\/italic applied uniformly across the whole run — an
-- embedded @w:drawing@ within a run (an image sharing a paragraph
-- with other text) is not specially recognised, for the same reason
-- 'paragraphIsStandaloneDrawing' only recognises a drawing alone.
runToRawInlines :: Cursor -> Either MirrorError [RawInline]
runToRawInlines r = pure (concatMap partToRawInline (r $/ checkName isTextOrBreak))
  where
    bold   = isRunPropertyOn "b" r
    italic = isRunPropertyOn "i" r
    isTextOrBreak n = n == wordNs "t" || n == wordNs "br"
    partToRawInline c
      | isNamed "br" c = [RawSoftBreak]
      | isNamed "t" c  =
          let txt = Text.concat (c $/ content)
          in if Text.null txt then [] else [formatRaw bold italic txt]
      | otherwise = []

formatRaw :: Bool -> Bool -> Text -> RawInline
formatRaw True  True  t = RawStrong [RawEmphasis [RawText t]]
formatRaw True  False t = RawStrong [RawText t]
formatRaw False True  t = RawEmphasis [RawText t]
formatRaw False False t = RawText t

-- | OOXML's @ST_OnOff@: a run property element's mere presence means
-- on, /unless/ it carries an explicit false-ish @w:val@.
isRunPropertyOn :: Text -> Cursor -> Bool
isRunPropertyOn propName r =
  case listToMaybe (r $// element (wordNs "rPr") &/ element (wordNs propName)) of
    Nothing         -> False
    Just propCursor -> case listToMaybe (attribute (wordNs "val") propCursor) of
      Nothing      -> True
      Just "0"     -> False
      Just "false" -> False
      Just "off"   -> False
      Just _       -> True
