-- | Markdown parsing.
--
-- The algorithm, decided before any code below was written: block
-- structure is recovered by a single left-to-right scan over source
-- lines. At each position, a fixed priority order of total,
-- independent recognisers decides what starts there — blank, fenced
-- code, thematic break, heading, a standalone image, block quote,
-- list item, table, or (falling through every other case) an
-- ordinary paragraph line — and each recogniser is responsible for
-- knowing how many further lines belong to it. This is still the
-- classify-and-group shape §6 originally established, extended with
-- more cases and, for constructs like fenced code and tables, a
-- little lookahead — never backtracking, never re-examining a line
-- once it has been consumed into a block. Inline content (emphasis,
-- strong, code spans, links) is delegated line-by-line — or, for a
-- multi-line paragraph, once for the whole joined block — to
-- "Mirror.Parser.Markdown.Inline", the one place this codebase uses a
-- combinator parser, because that is genuinely nested grammar in a
-- way block structure is not.
--
-- Stated scope, each a deliberate simplification rather than an
-- oversight:
--
--   * Lists are flat. A continuation line (non-blank, not itself a
--     new marker or other construct) extends the current item; a
--     line starting a nested sub-list at a deeper indentation is not
--     specially recognised — it becomes a new top-level list instead.
--   * A combined @***strong and emphasis***@ delimiter run is not
--     resolved as one construct (§ Inline module note) — write
--     @**_this_**@.
--   * A fenced code block's closing fence is any line that is,
--     stripped, three or more backticks — it need not match the
--     opening fence's exact backtick count.
--   * A table's column delimiter row must immediately follow the
--     header row; alignment colons (@:---@, @:--:@) are recognised as
--     syntax but not carried into the IR, which has no per-column
--     alignment field.
--   * A link or image destination containing a literal, unescaped
--     @)@ is not supported.
module Mirror.Parser.Markdown (parseMarkdown) where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text

import Mirror.Document (Document, headingLevelFromInt, mkDocument)
import Mirror.Document.Raw
import Mirror.Error
import Mirror.Parser.Markdown.Inline (inlinesFromSubstring)

-- | Total: every 'Text' input yields either a validated 'Document' or
-- a structured 'MirrorError'. Line-level recognition below never
-- rejects input by itself; the only possible failure is
-- 'Mirror.Error.EmptyDocumentBody' from 'mkDocument', or a validation
-- failure from 'toBlock' on content that parsed as some construct's
-- shape but failed its semantic check (an invalid link URL, a ragged
-- table) — the same checks every other source is held to (§3).
parseMarkdown :: Text -> Either MirrorError Document
parseMarkdown src = do
  blocks <- traverse (toBlock 0) (blocksFromLines (toLines src))
  mkDocument Nothing blocks

toLines :: Text -> [Text]
toLines = Text.lines . Text.replace "\r\n" "\n"

--------------------------------------------------------------------------
-- Line-level recognisers — each total and independent, deciding
-- whether one line (or, for isTableStart, one line plus a lookahead
-- at the next) starts a particular construct.
--------------------------------------------------------------------------

isBlank :: Text -> Bool
isBlank = Text.null . Text.strip

-- | Returns the raw hash count (not yet range-checked into a
-- 'Mirror.Document.HeadingLevel' — that happens once, in 'toBlock',
-- via 'headingLevelFromInt') alongside the title text. Also uses
-- 'headingLevelFromInt' here, discarding its result, purely as the
-- 1..6 gate that decides whether this is a heading line /at all/:
-- seven or more hashes is simply not a heading marker, falling
-- through to an ordinary paragraph line, not a rejected document.
headingMarker :: Text -> Maybe (Int, Text)
headingMarker line = do
  let (hashes, afterHashes) = Text.span (== '#') line
      n = Text.length hashes
  _         <- headingLevelFromInt n
  titleText <- Text.stripPrefix " " afterHashes
  pure (n, titleText)

-- | Three or more of the same character among @-@, @*@, @_@, ignoring
-- spaces, and nothing else on the line.
isThematicBreak :: Text -> Bool
isThematicBreak line = case Text.uncons compact of
  Just (c, _) | c `elem` ("-*_" :: String) -> Text.length compact >= 3 && Text.all (== c) compact
  _                                        -> False
  where
    compact = Text.filter (/= ' ') line

-- | A line consisting of nothing but @![alt](url)@ — see
-- "Mirror.Parser.Markdown.Inline"'s module note on why images are
-- recognised here rather than inline.
imageLine :: Text -> Maybe (Text, Text)
imageLine line = do
  afterBang   <- Text.stripPrefix "!" (Text.strip line)
  afterOpenB  <- Text.stripPrefix "[" afterBang
  let (altText, afterAlt) = Text.breakOn "]" afterOpenB
  afterCloseB <- Text.stripPrefix "]" afterAlt
  afterOpenP  <- Text.stripPrefix "(" afterCloseB
  let (url, afterUrl) = Text.breakOn ")" afterOpenP
  afterCloseP <- Text.stripPrefix ")" afterUrl
  if isBlank afterCloseP then Just (url, altText) else Nothing

-- | The language named on an opening fence, possibly empty. Requires
-- three or more backticks; leading whitespace before them is ignored.
fenceMarker :: Text -> Maybe Text
fenceMarker line =
  let (backticks, afterTicks) = Text.span (== '`') (Text.stripStart line)
  in if Text.length backticks >= 3 then Just (Text.strip afterTicks) else Nothing

isFenceClose :: Text -> Bool
isFenceClose line =
  let stripped = Text.strip line
  in Text.length stripped >= 3 && Text.all (== '`') stripped

-- | Strips a leading @">"@, and then one following space if present.
blockQuotePrefix :: Text -> Maybe Text
blockQuotePrefix line = do
  rest <- Text.stripPrefix ">" (Text.stripStart line)
  pure (fromMaybe rest (Text.stripPrefix " " rest))

data ListMarkerKind = UnorderedMarker | OrderedMarker deriving Eq

markerKindText :: ListMarkerKind -> Text
markerKindText UnorderedMarker = "unordered"
markerKindText OrderedMarker   = "ordered"

-- | @-@, @*@, or @+@ for an unordered item; one or more digits then
-- @.@ or @)@ for an ordered one — either way, followed by exactly one
-- space, then the item's own first line of text.
listItemMarker :: Text -> Maybe (ListMarkerKind, Text)
listItemMarker line = unordered trimmed <|> ordered trimmed
  where
    trimmed = Text.stripStart line
    unordered t = do
      rest0 <- foldr (\c acc -> Text.stripPrefix (Text.singleton c) t <|> acc) Nothing ['-', '*', '+']
      rest  <- Text.stripPrefix " " rest0
      pure (UnorderedMarker, rest)
    ordered t =
      let (digits, afterDigits) = Text.span isDigit t
      in if Text.null digits
           then Nothing
           else do
             afterPunct <- Text.stripPrefix "." afterDigits <|> Text.stripPrefix ")" afterDigits
             rest       <- Text.stripPrefix " " afterPunct
             pure (OrderedMarker, rest)

-- | Splits a pipe-delimited row into trimmed cells, discarding one
-- leading and one trailing empty cell caused by a row that itself
-- starts or ends with @|@ (the usual style, @"| a | b |"@). Escaped
-- pipes within a cell are not supported — a stated simplification.
splitTableRow :: Text -> [Text]
splitTableRow line = map Text.strip (Text.splitOn "|" core)
  where
    trimmed  = Text.strip line
    stripFront = fromMaybe trimmed (Text.stripPrefix "|" trimmed)
    core       = fromMaybe stripFront (Text.stripSuffix "|" stripFront)

isDelimiterRow :: Text -> Bool
isDelimiterRow line =
  let cells = splitTableRow line
  in not (null cells) && all isDelimiterCell cells
  where
    isDelimiterCell c =
      let stripped = Text.dropWhile (== ':') (Text.dropWhileEnd (== ':') c)
      in not (Text.null stripped) && Text.all (== '-') stripped

-- | A table's header row must contain @|@, and the line immediately
-- following it must be a valid delimiter row — the lookahead that
-- distinguishes a real table from an ordinary paragraph line that
-- merely happens to contain a @|@.
isTableStart :: Text -> [Text] -> Bool
isTableStart headerLine remaining =
  Text.isInfixOf "|" headerLine && case remaining of
    (next : _) -> isDelimiterRow next
    []         -> False

-- | Any line that does not start one of the other constructs —
-- shared between ordinary paragraph continuation and list-item
-- continuation, since both mean the same thing: "this line's text
-- belongs to whatever came before it, not to something new."
isPlainContinuation :: Text -> Bool
isPlainContinuation x =
  not (isBlank x)
    && not (isThematicBreak x)
    && isNothing (fenceMarker x)
    && isNothing (headingMarker x)
    && isNothing (imageLine x)
    && isNothing (blockQuotePrefix x)
    && isNothing (listItemMarker x)

--------------------------------------------------------------------------
-- The scan
--------------------------------------------------------------------------

blocksFromLines :: [Text] -> [RawBlock]
blocksFromLines [] = []
blocksFromLines (l : ls)
  | isBlank l
  = blocksFromLines ls

  | Just lang <- fenceMarker l
  = let (codeLines, afterFence) = break isFenceClose ls
        rest                    = drop 1 afterFence
        code                    = Text.intercalate "\n" codeLines
        maybeLang               = if Text.null lang then Nothing else Just lang
    in RawCodeBlock maybeLang code : blocksFromLines rest

  | isThematicBreak l
  = RawHorizontalRule : blocksFromLines ls

  | Just (levelInt, titleText) <- headingMarker l
  = RawHeading levelInt (inlinesFromSubstring titleText) : blocksFromLines ls

  | Just (url, alt) <- imageLine l
  = RawImage url alt Nothing : blocksFromLines ls

  | Just firstUnwrapped <- blockQuotePrefix l
  = let (quoteLines, rest) = spanBlockQuote ls
    in RawBlockQuote (blocksFromLines (firstUnwrapped : quoteLines)) : blocksFromLines rest

  | Just (kind, firstItemText) <- listItemMarker l
  = let (itemTexts, rest) = spanListItems kind firstItemText ls
        toItem t = [RawParagraph (inlinesFromSubstring t)]
    in RawList (markerKindText kind) (map toItem itemTexts) : blocksFromLines rest

  | isTableStart l ls
  = let headerCells       = splitTableRow l
        afterDelimiter    = drop 1 ls
        isBodyRow x       = not (isBlank x) && Text.isInfixOf "|" x
        (bodyLines, rest) = span isBodyRow afterDelimiter
    in RawTable (Just (rowFromCells headerCells)) (map (rowFromCells . splitTableRow) bodyLines)
         : blocksFromLines rest

  | otherwise
  = let (contentLines, rest) = spanPlain (l : ls)
    in RawParagraph (inlinesFromSubstring (Text.intercalate "\n" contentLines)) : blocksFromLines rest

  where
    rowFromCells = map inlinesFromSubstring

-- | Consumes consecutive block-quote-prefixed lines (stopping at the
-- first line that isn't one — a blank line ends the quote, matching
-- the same "no lazy continuation" simplification used throughout),
-- unwrapping each one line at a time.
spanBlockQuote :: [Text] -> ([Text], [Text])
spanBlockQuote (x : xs) = case blockQuotePrefix x of
  Just unwrapped -> let (more, rest) = spanBlockQuote xs in (unwrapped : more, rest)
  Nothing        -> ([], x : xs)
spanBlockQuote [] = ([], [])

-- | Consumes items of one list: a line matching a marker of the same
-- 'ListMarkerKind' starts a new item; any other 'isPlainContinuation'
-- line extends the current item (joined with a newline, the same way
-- a paragraph's lines are); anything else ends the list.
spanListItems :: ListMarkerKind -> Text -> [Text] -> ([Text], [Text])
spanListItems kind firstItemText = go [firstItemText]
  where
    go acc (x : xs)
      | Just (kind', itemText) <- listItemMarker x, kind' == kind
      = go (itemText : acc) xs
      | isPlainContinuation x
      = case acc of
          (cur : more) -> go ((cur <> "\n" <> Text.stripStart x) : more) xs
          []           -> go acc xs
      | otherwise
      = (reverse acc, x : xs)
    go acc [] = (reverse acc, [])

-- | Consumes a maximal run of ordinary paragraph lines, starting from
-- (and including) a line already known to be one. Each continuation
-- line's own leading whitespace is stripped before joining — it is
-- incidental alignment, not content, the same judgment applied to
-- list-item continuation lines above.
spanPlain :: [Text] -> ([Text], [Text])
spanPlain (x : xs)
  | isPlainContinuation x = let (more, rest) = spanPlain xs in (Text.stripStart x : more, rest)
spanPlain xs = ([], xs)
