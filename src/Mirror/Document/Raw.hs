-- | A staging representation of 'Mirror.Document.Block' and
-- 'Mirror.Document.Inline' whose leaves are not yet validated — a raw
-- 'Int' heading level instead of a checked 'Mirror.Document.HeadingLevel',
-- a raw 'Data.Text.Text' href instead of a checked 'Mirror.Document.Url',
-- and so on.
--
-- This exists because three different parsers now reach a point where
-- they have recovered a document's *shape* — from a JSON tag, an XML
-- element, or a Markdown block/delimiter — before they know whether
-- its *content* is valid. 'Mirror.Parser.Json' (§8) was first to need
-- this split; 'Mirror.Parser.Markdown' (§6) and 'Mirror.Parser.Docx'
-- (§7) now share it rather than each re-deriving their own version of
-- "is this URL valid", "is this table rectangular", "is this heading
-- level in range". A ragged table is 'Mirror.Error.MismatchedTableColumns'
-- identically regardless of which of the three parsers built the
-- 'RawBlock' that exposed it, because all three eventually call the
-- same 'toBlock'.
module Mirror.Document.Raw
  ( RawBlock (..)
  , RawInline (..)
  , RawCell
  , RawRow
  , toBlock
  , toInline
  , toListItem
  , toLanguageTag
  ) where

import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import qualified Data.Text as Text

import Mirror.Document
import Mirror.Error

-- | A 'Block' with every constructor's shape but unvalidated leaves.
data RawBlock
  = RawHeading Int [RawInline]
  | RawParagraph [RawInline]
  | RawList Text [[RawBlock]]
  | RawTable (Maybe RawRow) [RawRow]
  | RawBlockQuote [RawBlock]
  | RawCodeBlock (Maybe Text) Text
  | RawImage Text Text
  | RawHorizontalRule
  deriving Show

-- | An 'Inline' with every constructor's shape but unvalidated leaves.
data RawInline
  = RawText Text
  | RawEmphasis [RawInline]
  | RawStrong [RawInline]
  | RawLink Text [RawInline]
  | RawInlineCode Text
  | RawSoftBreak
  deriving Show

type RawCell = [RawInline]
type RawRow  = [RawCell]

toBlock :: RawBlock -> Either MirrorError Block
toBlock (RawHeading levelInt rawInlines) = do
  level   <- note (ErrParse (JsonSchemaViolation (badHeadingLevel levelInt))) (headingLevelFromInt levelInt)
  inlines <- traverse toInline rawInlines
  pure (Heading level inlines)
toBlock (RawParagraph rawInlines) =
  Paragraph <$> traverse toInline rawInlines
toBlock (RawList kindText rawItems) = do
  kind    <- note (ErrParse (JsonSchemaViolation (badListKind kindText))) (listKindFromText kindText)
  items   <- traverse toListItem rawItems
  itemsNE <- note (ErrValidation EmptyList) (nonEmpty items)
  pure (BulletList kind itemsNE)
toBlock (RawTable rawHeader rawRows) = do
  header <- traverse toRawCells rawHeader
  rows   <- traverse toRawCells rawRows
  mkTable header rows
  where
    toRawCells = traverse (traverse toInline)
toBlock (RawBlockQuote rawBlocks) = do
  blocks   <- traverse toBlock rawBlocks
  blocksNE <- note (ErrValidation EmptyBlockQuote) (nonEmpty blocks)
  pure (BlockQuote blocksNE)
toBlock (RawCodeBlock rawLanguage code) = do
  language <- traverse toLanguageTag rawLanguage
  pure (CodeBlock language code)
toBlock (RawImage src alt) = do
  url <- mkUrl src
  pure (Image url alt)
toBlock RawHorizontalRule = pure HorizontalRule

toListItem :: [RawBlock] -> Either MirrorError ListItem
toListItem rawBlocks = ListItem <$> traverse toBlock rawBlocks

toLanguageTag :: Text -> Either MirrorError LanguageTag
toLanguageTag t = note (ErrValidation (InvalidLanguageTag t)) (languageTagFromText t)

toInline :: RawInline -> Either MirrorError Inline
toInline (RawText t)       = pure (Text t)
toInline (RawEmphasis xs)  = Emphasis <$> traverse toInline xs
toInline (RawStrong xs)    = Strong <$> traverse toInline xs
toInline (RawLink href xs) = Link <$> mkUrl href <*> traverse toInline xs
toInline (RawInlineCode t) = pure (InlineCode t)
toInline RawSoftBreak      = pure SoftBreak

badHeadingLevel :: Int -> Text
badHeadingLevel n = "heading \"level\" must be an integer from 1 to 6, got " <> Text.pack (show n)

badListKind :: Text -> Text
badListKind t = "list \"kind\" must be \"ordered\" or \"unordered\", got " <> t
