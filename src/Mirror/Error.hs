-- | The complete, closed vocabulary of things that can go wrong anywhere
-- in Mirror.
--
-- Reachability discipline: a constructor belongs here only if some
-- exported function — either a parser reachable from the CLI, or a
-- smart constructor in "Mirror.Document" that any direct caller of the
-- library could trigger — can actually produce it. A feature that does
-- not exist yet does not get an error constructor in anticipation of
-- itself; the constructor is added in the same change that makes it
-- reachable. A closed vocabulary that includes cases nothing can ever
-- return is not more honest than an open one — it is a different way
-- of lying about completeness.
module Mirror.Error
  ( MirrorError (..)
  , ParseError (..)
  , ValidationError (..)
  , RenderError (..)
  , IOFailure (..)
  , renderMirrorError
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | The four failure categories, one per pipeline stage (parse, then
-- validate, then render, plus I\/O around all of it). A closed sum of
-- closed sums: nothing downstream ever pattern-matches on an open
-- string tag or reaches for a catch-all constructor.
data MirrorError
  = ErrParse ParseError
  | ErrValidation ValidationError
  | ErrRender RenderError
  | ErrIO IOFailure
  deriving (Eq, Show)

-- | All constructors below are positional, not record syntax. A record
-- field shared across constructors that don't all have it generates a
-- partial accessor — exactly the partiality this project forbids.
-- Positional fields plus pattern matching in the renderers below avoid
-- that trap uniformly, in every category.
data ParseError
  = MarkdownInvalidEncoding Text
    -- ^ The Markdown source is not valid UTF-8. This is the only
    -- Markdown-specific parse failure that exists, because Markdown
    -- block structure (§6) is recovered by classifying and grouping
    -- already-decoded lines — a total operation with no notion of
    -- "malformed input" left in it once decoding has succeeded. A
    -- syntax-error case for Markdown will return if inline markup
    -- (emphasis, links) is added later, scoped to that inline grammar.
  | JsonSyntax Text
    -- ^ The input is not well-formed JSON at all (aeson's own
    -- diagnostic, which already covers encoding problems — invalid
    -- UTF-8 is simply invalid JSON, so there is no separate encoding
    -- error for this format the way there is for Markdown).
  | JsonSchemaViolation Text
    -- ^ The input is well-formed JSON but does not conform to Mirror's
    -- document schema (§8) — an unrecognised @"type"@ tag, a missing
    -- required key, or a key whose JSON type doesn't match what the
    -- schema requires at that position.
  | UnsupportedSourceExtension FilePath Text
    -- ^ Input path, and the unrecognised extension found on it.
  | DocxNotAZipArchive FilePath
  | DocxMissingPart Text
    -- ^ Name of the required zip entry that was absent.
  | DocxMalformedXml Text Text
    -- ^ Part name, underlying parser message.
  | DocxUnresolvedRelationship Text
    -- ^ A @r:id@\/@r:embed@ reference (hyperlink or image) had no
    -- matching entry in @word\/_rels\/document.xml.rels@.
  | DocxUnresolvedNumbering Text
    -- ^ A paragraph's @w:numId@ had no matching, resolvable format in
    -- @word\/numbering.xml@ — a distinct failure from an unresolved
    -- relationship: numbering and relationships are separate OOXML
    -- subsystems with separate parts.
  | DocxUnsupportedImageFormat Text
    -- ^ An embedded image's file extension isn't one of the closed,
    -- recognised set (§7) Mirror knows how to embed as a data URI.
  | DocxPartTooLarge Text Integer Integer
    -- ^ Part name, its declared uncompressed size, and the configured
    -- ceiling it exceeded. Reported before the part is fully
    -- decompressed, guarding against a small, maliciously crafted zip
    -- entry expanding into an unbounded amount of memory (a "zip
    -- bomb") — the same "resource bound, checked, not assumed"
    -- discipline already applied to recursive-structure nesting.
  | DocxTooManyImages Int Int
    -- ^ Number of distinct embedded images found, and the configured
    -- ceiling — guards against a document referencing an unbounded
    -- number of images each requiring a full in-memory base64 copy.
  | ExcessiveNestingDepth Int
    -- ^ A parsed document's block\/inline structure exceeded Mirror's
    -- nesting-depth ceiling /before/ rendering — checked at parse time
    -- (unlike 'Mirror.Error.RenderError''s 'UnrenderableNestingDepth',
    -- which guards the render stage) because 'Mirror.Document.Raw'
    -- and the Markdown inline grammar both recursively materialise a
    -- document's full tree and are each independently capable of
    -- exhausting the stack on adversarial input, prior to any
    -- rendering step running at all.
  deriving (Eq, Show)

data ValidationError
  = EmptyDocumentTitle
    -- ^ A title was supplied but is empty or all-whitespace.
  | EmptyDocumentBody
    -- ^ A document was assembled with zero blocks.
  | EmptyTable
    -- ^ A table was assembled with zero body rows. Kept distinct from
    -- 'EmptyTableRow': this is about row *count*, that one is about
    -- cell count within a single row. An earlier design folded both
    -- into one constructor, which meant "the table has no rows at all"
    -- was reported to the user as "a row needs at least one cell" —
    -- true of the symptom, silent about the actual cause.
  | EmptyTableRow
    -- ^ One specific table row was assembled with zero cells.
  | EmptyList
    -- ^ A bulleted\/numbered list was assembled with zero items.
  | EmptyBlockQuote
    -- ^ A block quote was assembled with zero blocks.
  | MismatchedTableColumns Int Int
    -- ^ Canonical (first-row) width, and the width of the offending row.
  | InvalidUrl Text
    -- ^ Not a syntactically valid URI at all.
  | DisallowedUrlScheme Text
    -- ^ A syntactically valid URI whose scheme is outside the
    -- relevant closed allow-list (§3) — 'Mirror.Document.mkUrl' (link
    -- hrefs) and 'Mirror.Document.mkImageUrl' (image srcs) enforce
    -- /different/ allow-lists, and this same constructor also covers
    -- a @data:@ URI whose own stated media type isn't an image.
    -- Kept distinct from 'InvalidUrl' because "malformed" and
    -- "syntactically fine but forbidden" are different facts a user
    -- needs to act on differently, the same distinction
    -- 'EmptyTable'\/'EmptyTableRow' already draws for tables.
  | InvalidLanguageTag Text
    -- ^ A code block named a fenced-code language outside Mirror's
    -- closed, recognised set (§3).
  | InvalidHeadingLevel Int
    -- ^ A heading's level fell outside 1..6. Format-independent: every
    -- source's raw heading level is checked identically by
    -- 'Mirror.Document.Raw.toBlock', so this is never tagged as a
    -- JSON-, Markdown-, or DOCX-specific failure even though only
    -- JSON's free-form @"level"@ field can produce it today.
  | EmptyLinkText Text
    -- ^ A hyperlink whose visible text is empty, and the @href@ it
    -- pointed to (so this is namable in a real document, not just a
    -- bare complaint — a converter that hard-fails a 40-page document
    -- over one bad link owes the reader /which/ link, per §2.15).
    -- Unlike a paragraph or a table cell, an anchor with no content is
    -- not "ordinary, possibly-empty data" — it is both inaccessible
    -- (no accessible name for assistive technology) and unusable
    -- (nothing to click).
  deriving (Eq, Show)

-- | Failures possible while turning a validated 'Mirror.Document.Document'
-- into HTML. Kept as its own category rather than folded into
-- 'ValidationError' because rendering (§9) is a different pipeline
-- stage, with a different caller, than validation (§3, exercised by
-- §6-§8) — a future renderer failure (an unresolvable cross-reference,
-- should Mirror grow reference-style links) belongs here, not there.
-- Only one case exists today; it is declared with @data@, not
-- @newtype@, because the category — not this specific case — is what's
-- stable.
data RenderError
  = UnrenderableNestingDepth Int
    -- ^ The document's block\/inline nesting exceeded the renderer's
    -- configured limit (§9). 'Mirror.Document.Block' and
    -- 'Mirror.Document.Inline' are recursive types, so a pathological
    -- or adversarially generated document can nest arbitrarily deep;
    -- rendering bounds that depth so it fails cleanly instead of
    -- exhausting the stack.
  deriving (Eq, Show)

data IOFailure
  = SourceFileNotFound FilePath
  | SourceFileUnreadable FilePath Text
  | OutputFileUnwritable FilePath Text
  | StylesheetUnreadable FilePath Text
  deriving (Eq, Show)

renderMirrorError :: MirrorError -> Text
renderMirrorError = \case
  ErrParse e      -> "Parse error: "      <> renderParseError e
  ErrValidation e -> "Validation error: " <> renderValidationError e
  ErrRender e     -> "Render error: "     <> renderRenderError e
  ErrIO e         -> "I/O error: "        <> renderIOFailure e

renderParseError :: ParseError -> Text
renderParseError = \case
  MarkdownInvalidEncoding msg ->
    "input is not valid UTF-8: " <> msg
  JsonSyntax diag ->
    "not well-formed JSON:\n" <> diag
  JsonSchemaViolation msg ->
    "JSON does not match Mirror's document schema: " <> msg
  UnsupportedSourceExtension path ext ->
    Text.pack path <> ": unrecognised file extension \"" <> ext
      <> "\" (expected .md, .markdown, .docx, or .json)"
  DocxNotAZipArchive fp ->
    Text.pack fp <> " is not a valid DOCX (zip) archive"
  DocxMissingPart part ->
    "DOCX archive is missing required part: " <> part
  DocxMalformedXml part err ->
    "malformed XML in " <> part <> ": " <> err
  DocxUnresolvedRelationship rid ->
    "reference to relationship \"" <> rid <> "\" has no matching entry in the document's relationships part"
  DocxUnresolvedNumbering numId ->
    "list numbering id \"" <> numId <> "\" has no resolvable format in the document's numbering part"
  DocxUnsupportedImageFormat ext ->
    "embedded image format \"" <> ext <> "\" is not one of Mirror's recognised set"
  DocxPartTooLarge part size limit ->
    "DOCX part \"" <> part <> "\" is " <> tshow size <> " bytes uncompressed, exceeding the "
      <> tshow limit <> "-byte limit"
  DocxTooManyImages count limit ->
    "DOCX document references " <> tshow count <> " embedded images, exceeding the "
      <> tshow limit <> "-image limit"
  ExcessiveNestingDepth d ->
    "nesting depth " <> tshow d <> " exceeds Mirror's limit while parsing the document"

renderValidationError :: ValidationError -> Text
renderValidationError = \case
  EmptyDocumentTitle -> "document title, if given, must not be empty"
  EmptyDocumentBody  -> "document must contain at least one block"
  EmptyTable         -> "a table must contain at least one row"
  EmptyTableRow      -> "a table row must contain at least one cell"
  EmptyList          -> "a list must contain at least one item"
  EmptyBlockQuote    -> "a block quote must contain at least one block"
  MismatchedTableColumns expected got ->
    "table row has " <> tshow got <> " columns, expected " <> tshow expected
  InvalidUrl u           -> "not a valid URL: " <> u
  DisallowedUrlScheme s  -> "URL scheme \"" <> s <> "\" is not permitted in this context"
  InvalidLanguageTag t   -> "not a recognised code-block language: " <> t
  InvalidHeadingLevel n  -> "heading level must be an integer from 1 to 6, got " <> tshow n
  EmptyLinkText href      -> "a link to \"" <> href <> "\" has no visible text"

renderRenderError :: RenderError -> Text
renderRenderError = \case
  UnrenderableNestingDepth d ->
    "nesting depth " <> tshow d <> " exceeds the renderer's limit"

renderIOFailure :: IOFailure -> Text
renderIOFailure = \case
  SourceFileNotFound fp      -> "input file not found: " <> Text.pack fp
  SourceFileUnreadable fp r  -> "cannot read " <> Text.pack fp <> ": " <> r
  OutputFileUnwritable fp r  -> "cannot write " <> Text.pack fp <> ": " <> r
  StylesheetUnreadable fp r  -> "cannot read stylesheet " <> Text.pack fp <> ": " <> r

tshow :: Show a => a -> Text
tshow = Text.pack . show
