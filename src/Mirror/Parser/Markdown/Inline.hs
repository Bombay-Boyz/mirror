{-# LANGUAGE OverloadedStrings #-}

-- | Inline Markdown parsing: emphasis, strong, inline code, and
-- links, within one line — or one already-line-joined paragraph,
-- heading, list item, or table cell's worth of text.
--
-- Images are deliberately not handled here: 'Mirror.Document.Image'
-- is a /block/-level constructor (matching how DOCX and JSON both
-- treat it, §3, §7, §8), so Markdown's @![alt](url)@ syntax is
-- recognised at the block level too, as a line consisting of nothing
-- else (§6) — not as something that can appear mid-paragraph.
--
-- The algorithm, decided before any code below was written: unlike
-- block structure (§6), inline structure genuinely nests — emphasis
-- inside a link, strong inside emphasis — so this is exactly the case
-- §6 itself names as where a combinator parser earns its place, and
-- it is the only place in this codebase Megaparsec is used.
--
-- The one correctness trap worth naming explicitly: a naive
-- recursive-descent search for a closing delimiter — recursively
-- trying to open /new/ spans while scanning for the delimiter that
-- closes the /current/ one — lets an unrelated inner delimiter steal
-- characters that belong to the outer closing marker. Concretely,
-- naively parsing @"**text*more**"@ by recursively invoking the full
-- inline grammar while hunting for the closing @"**"@ can mis-consume
-- the lone interior @*@ as the opening of a nested emphasis span,
-- corrupting where the outer @"**"@ is found. The fix is to separate
-- the two phases: first scan forward, character by character, purely
-- to find the raw text up to the next occurrence of the closing
-- delimiter (no attempt to open anything while doing this); only once
-- that span's exact boundaries are known is its captured substring
-- recursively parsed, as a fresh, independent parse, by
-- 'inlinesFromSubstring'. That separation is what makes the interior
-- @*@ in the example above correctly fall out as literal text once
-- the true close is found, rather than being mistaken for the start
-- of a new span.
module Mirror.Parser.Markdown.Inline (inlinesFromSubstring) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char)

import Mirror.Document.Raw

type Parser = Parsec Void Text

-- | Total. See the module note for why: 'inlineElementP''s ultimate
-- fallback, 'literalCharP', consumes exactly one character
-- unconditionally, so 'many inlineElementP' can only stop at true end
-- of input, at which point the trailing 'eof' always succeeds. The
-- 'Left' branch below is consequently expected to be unreachable; it
-- is handled with a safe, non-crashing fallback (the substring as a
-- single literal run of text) rather than assumed away with an
-- unsafe function — Megaparsec's own type honestly admits failure,
-- and this project does not paper over that with partiality (§0,
-- rule 1), even for a case reasoned to be impossible.
inlinesFromSubstring :: Text -> [RawInline]
inlinesFromSubstring sub = case parse (many inlineElementP <* eof) "" sub of
  Right inlines -> mergeAdjacentText inlines
  Left _        -> [RawText sub]

inlineElementP :: Parser RawInline
inlineElementP = choice
  [ try linkP
  , try codeSpanP
  , try (strongP '*')
  , try (strongP '_')
  , try (emphasisP '*')
  , try (emphasisP '_')
  , try escapedCharP
  , softBreakP
  , literalCharP
  ]

-- | A literal @'\n'@ stands for a soft line break within a
-- line-joined paragraph (§6) — inserted by the block-level module,
-- never present in a genuine single-line source such as a table cell.
softBreakP :: Parser RawInline
softBreakP = RawSoftBreak <$ char '\n'

-- | A backslash makes the following character literal, whatever it
-- is — covering the common cases (@\\*@, @\\_@, @\\[@, @\\\\@) without
-- restricting to a fixed punctuation set.
escapedCharP :: Parser RawInline
escapedCharP = RawText . Text.singleton <$> (char '\\' *> anySingle)

-- | Never recursively parsed — a code span's content is shielded from
-- further Markdown interpretation, matching every Markdown dialect's
-- convention. Only single backticks are recognised as fences; a run
-- of two or more backticks (CommonMark's mechanism for including a
-- literal backtick inside a code span) is not supported, a deliberate,
-- stated simplification.
codeSpanP :: Parser RawInline
codeSpanP = do
  _   <- char '`'
  raw <- rawUntil (char '`')
  pure (RawInlineCode raw)

-- | @**strong**@ or @__strong__@. See the module note: the search for
-- the closing pair is a plain character scan, not a recursive
-- invocation of 'inlineElementP'.
strongP :: Char -> Parser RawInline
strongP delim = do
  _   <- count 2 (char delim)
  raw <- rawUntil (count 2 (char delim))
  pure (RawStrong (inlinesFromSubstring raw))

-- | @*emphasis*@ or @_emphasis_@. Combined @***strong and emphasis***@
-- in one delimiter run is not specially resolved — write
-- @**_this_**@ instead — a deliberate simplification short of full
-- CommonMark delimiter-run precedence, which this module does not
-- attempt to reproduce.
emphasisP :: Char -> Parser RawInline
emphasisP delim = do
  _   <- char delim
  raw <- rawUntil (char delim)
  pure (RawEmphasis (inlinesFromSubstring raw))

-- | Link text is recursively parsed (so @[**bold** link](url)@
-- works); the destination is taken verbatim and validated later, by
-- the same 'Mirror.Document.mkUrl' every other source uses (§3).
-- A destination containing a literal, unescaped @)@ is not supported —
-- wrap it in a shorter link or accept the simplification.
linkP :: Parser RawInline
linkP = do
  _       <- char '['
  altText <- rawUntil (char ']')
  _       <- char '('
  url     <- rawUntil (char ')')
  pure (RawLink url (inlinesFromSubstring altText))

-- | The one place 'many'\/'eof' is not the terminating combinator:
-- 'literalCharP' is tried only after every special construct above
-- has failed to match at this position, and unconditionally consumes
-- one character — see the module note on why this makes the grammar
-- total.
literalCharP :: Parser RawInline
literalCharP = RawText . Text.singleton <$> anySingle

-- | Scans forward character by character for @end@, without
-- attempting to open any nested construct while doing so — the
-- separation described in the module note. Returns everything before
-- @end@; @end@ itself is consumed but discarded.
rawUntil :: Parser end -> Parser Text
rawUntil end = Text.pack <$> manyTill anySingle (try end)

-- | Collapses adjacent 'RawText' runs produced by 'literalCharP'
-- consuming one character at a time, so @"abc"@ round-trips as one
-- node rather than three.
mergeAdjacentText :: [RawInline] -> [RawInline]
mergeAdjacentText (RawText a : RawText b : rest) = mergeAdjacentText (RawText (a <> b) : rest)
mergeAdjacentText (x : rest)                     = x : mergeAdjacentText rest
mergeAdjacentText []                             = []
