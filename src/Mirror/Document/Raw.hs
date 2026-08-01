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

import Mirror.Document
import Mirror.Error

-- | Applied uniformly to every recursive descent in this module — the
-- same ceiling 'Mirror.Renderer.Html' enforces at the render stage,
-- but enforced here too because this is where 'RawBlock'\/'RawInline'
-- (already fully recursive trees, however they were produced — a
-- deeply nested JSON payload, or a Markdown inline grammar recursing
-- on captured substrings) are first converted to 'Block'\/'Inline'.
-- Checking depth only in the renderer left this exact stage unguarded:
-- a document nested deeper than the renderer's own limit would
-- exhaust the stack /here/, before rendering ever got a chance to
-- reject it cleanly.
maxRawNestingDepth :: Int
maxRawNestingDepth = 64

checkRawDepth :: Int -> Either MirrorError ()
checkRawDepth depth
  | depth > maxRawNestingDepth = Left (ErrParse (ExcessiveNestingDepth depth))
  | otherwise                  = Right ()

-- | A 'Block' with every constructor's shape but unvalidated leaves.
data RawBlock
  = RawHeading Int [RawInline]
  | RawParagraph [RawInline]
  | RawList Text [[RawBlock]]
  | RawTable (Maybe RawRow) [RawRow]
  | RawBlockQuote [RawBlock]
  | RawCodeBlock (Maybe Text) Text
  | RawImage Text Text (Maybe (Int, Int))
    -- ^ Src, alt, and optional pixel dimensions — 'Just' only from a
    -- source format that states them explicitly (DOCX's @wp:extent@).
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

-- | Every recursive descent below is depth-checked at entry via
-- 'checkRawDepth', and every recursive call passes @depth + 1@ —
-- applied uniformly across every constructor capable of nesting
-- ('RawBlockQuote', 'RawList' items, 'RawTable' cells, 'RawEmphasis'\/
-- 'RawStrong'\/'RawLink' in 'toInline'), not just the ones an author
-- happened to think of first.
toBlock :: Int -> RawBlock -> Either MirrorError Block
toBlock depth raw = do
  checkRawDepth depth
  case raw of
    RawHeading levelInt rawInlines -> do
      level   <- note (ErrValidation (InvalidHeadingLevel levelInt)) (headingLevelFromInt levelInt)
      inlines <- traverse (toInline (depth + 1)) rawInlines
      pure (Heading level inlines)
    RawParagraph rawInlines ->
      Paragraph <$> traverse (toInline (depth + 1)) rawInlines
    RawList kindText rawItems -> do
      kind    <- note (ErrParse (JsonSchemaViolation (badListKind kindText))) (listKindFromText kindText)
      items   <- traverse (toListItem (depth + 1)) rawItems
      itemsNE <- note (ErrValidation EmptyList) (nonEmpty items)
      pure (BulletList kind itemsNE)
    RawTable rawHeader rawRows -> do
      header <- traverse (toRawCells (depth + 1)) rawHeader
      rows   <- traverse (toRawCells (depth + 1)) rawRows
      mkTable header rows
    RawBlockQuote rawBlocks -> do
      blocks   <- traverse (toBlock (depth + 1)) rawBlocks
      blocksNE <- note (ErrValidation EmptyBlockQuote) (nonEmpty blocks)
      pure (BlockQuote blocksNE)
    RawCodeBlock rawLanguage code -> do
      language <- traverse toLanguageTag rawLanguage
      pure (CodeBlock language code)
    RawImage src alt dims -> do
      url <- mkImageUrl src
      pure (Image url alt dims)
    RawHorizontalRule -> pure HorizontalRule
  where
    toRawCells d = traverse (traverse (toInline d))

toListItem :: Int -> [RawBlock] -> Either MirrorError ListItem
toListItem depth rawBlocks = ListItem <$> traverse (toBlock depth) rawBlocks

toLanguageTag :: Text -> Either MirrorError LanguageTag
toLanguageTag t = note (ErrValidation (InvalidLanguageTag t)) (languageTagFromText t)

toInline :: Int -> RawInline -> Either MirrorError Inline
toInline depth raw = do
  checkRawDepth depth
  case raw of
    RawText t       -> pure (Plain t)
    RawEmphasis xs  -> Emphasis <$> traverse (toInline (depth + 1)) xs
    RawStrong xs    -> Strong <$> traverse (toInline (depth + 1)) xs
    RawLink href xs -> do
      url       <- mkUrl href
      inlines   <- traverse (toInline (depth + 1)) xs
      inlinesNE <- note (ErrValidation EmptyLinkText) (nonEmpty inlines)
      pure (Link url inlinesNE)
    RawInlineCode t -> pure (InlineCode t)
    RawSoftBreak    -> pure SoftBreak

badListKind :: Text -> Text
badListKind t = "list \"kind\" must be \"ordered\" or \"unordered\", got " <> t
