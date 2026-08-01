-- | JSON parsing.
--
-- The algorithm, decided before any code below was written: JSON is
-- Mirror's own canonical interchange format — a direct, tagged-union
-- encoding of "Mirror.Document"'s IR (§3), documented in full at the
-- bottom of this module — rather than an inference over arbitrary
-- JSON shapes the way, say, a spreadsheet importer might guess a
-- table from an array of records. That framing is what makes the rest
-- of the design fall out directly: decoding happens in two strictly
-- separated phases, mirroring the two kinds of question "is this
-- input acceptable" can mean.
--
--   Phase 1 (this module, using @aeson@'s 'Parser'): is the JSON the
--   /shape/ Mirror's schema describes — the right keys, the right
--   JSON value types at each position, a recognised @"type"@ tag at
--   every block and inline? This phase produces
--   'Mirror.Document.Raw.RawBlock'\/'Mirror.Document.Raw.RawInline' —
--   trees with exactly Mirror's shape, but with leaf fields still raw
--   ('Int', 'Text') rather than validated ('HeadingLevel', 'Url').
--
--   Phase 2 ("Mirror.Document.Raw", shared with §6 and §7): do the
--   leaf values make sense as a document — is a URL syntactically
--   valid, is a table rectangular, is a heading level in range? A
--   JSON table with ragged rows fails with
--   'Mirror.Error.MismatchedTableColumns' — the identical error a
--   ragged Markdown or DOCX table produces — because it goes through
--   the identical function.
--
-- Keeping these separate means a JSON-specific concern (an
-- unrecognised @"type"@ tag) and a format-independent one (an invalid
-- URL) are never confused with each other, and it means every
-- constructor Mirror's IR defines is reachable through this parser
-- alone, even where Markdown's or DOCX's own grammars are narrower.
--
-- The schema this module accepts is specified in full in §8.
module Mirror.Parser.Json (parseJson) where

import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Types (Parser, parseEither, withArray, withObject, (.:), (.:?))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Mirror.Document (Document, mkDocument, mkNonEmptyText)
import Mirror.Document.Raw
import Mirror.Error

-- | Total in the sense every other parser is: every input yields
-- either a validated 'Mirror.Document.Document' or a structured
-- 'MirrorError', never an exception.
parseJson :: BSL.ByteString -> Either MirrorError Document
parseJson bytes = do
  value               <- first (ErrParse . JsonSyntax . Text.pack) (eitherDecode bytes)
  (rawTitle, rawBody) <- first (ErrParse . JsonSchemaViolation . Text.pack)
                                (parseEither rawDocumentParser value)
  title  <- traverse mkNonEmptyText rawTitle
  blocks <- traverse (toBlock 0) rawBody
  mkDocument title blocks

--------------------------------------------------------------------------
-- JSON shape, via aeson — produces Mirror.Document.Raw's types
--------------------------------------------------------------------------

rawDocumentParser :: Value -> Parser (Maybe Text, [RawBlock])
rawDocumentParser = withObject "document" $ \o -> do
  rawTitle  <- o .:? "title"
  blockVals <- o .: "blocks"
  rawBody   <- traverse rawBlockParser blockVals
  pure (rawTitle, rawBody)

rawBlockParser :: Value -> Parser RawBlock
rawBlockParser = withObject "block" $ \o -> do
  blockType <- o .: "type"
  case (blockType :: Text) of
    "heading" -> do
      level       <- o .: "level"
      contentVals <- o .: "content"
      RawHeading level <$> traverse rawInlineParser contentVals
    "paragraph" -> do
      contentVals <- o .: "content"
      RawParagraph <$> traverse rawInlineParser contentVals
    "list" -> do
      kindText <- o .: "kind"
      itemVals <- o .: "items"
      RawList kindText <$> traverse (traverse rawBlockParser) itemVals
    "table" -> do
      headerVal <- o .:? "header"
      header    <- traverse rawRowParser headerVal
      rowVals   <- o .: "rows"
      rows      <- traverse rawRowParser rowVals
      pure (RawTable header rows)
    "blockquote" -> do
      contentVals <- o .: "content"
      RawBlockQuote <$> traverse rawBlockParser contentVals
    "code_block" -> do
      language <- o .:? "language"
      code     <- o .: "code"
      pure (RawCodeBlock language code)
    "image" -> do
      src    <- o .: "src"
      alt    <- o .: "alt"
      width  <- o .:? "width"
      height <- o .:? "height"
      pure (RawImage src alt ((,) <$> width <*> height))
    "rule" -> pure RawHorizontalRule
    other  -> fail ("unrecognised block \"type\": " <> Text.unpack other)

rawInlineParser :: Value -> Parser RawInline
rawInlineParser = withObject "inline" $ \o -> do
  inlineType <- o .: "type"
  case (inlineType :: Text) of
    "text" -> RawText <$> o .: "text"
    "emphasis" -> do
      contentVals <- o .: "content"
      RawEmphasis <$> traverse rawInlineParser contentVals
    "strong" -> do
      contentVals <- o .: "content"
      RawStrong <$> traverse rawInlineParser contentVals
    "link" -> do
      href        <- o .: "href"
      contentVals <- o .: "content"
      RawLink href <$> traverse rawInlineParser contentVals
    "code_span" -> RawInlineCode <$> o .: "text"
    "break"     -> pure RawSoftBreak
    other       -> fail ("unrecognised inline \"type\": " <> Text.unpack other)

rawCellParser :: Value -> Parser RawCell
rawCellParser = withArray "cell" (traverse rawInlineParser . Vector.toList)

rawRowParser :: Value -> Parser RawRow
rawRowParser = withArray "row" (traverse rawCellParser . Vector.toList)
