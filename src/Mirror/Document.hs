-- | The canonical, format-independent document IR, and the only
-- API — the smart constructors below — capable of building the parts
-- of it that carry a runtime invariant.
--
-- Design rule for choosing 'NonEmpty' versus a plain list, applied
-- uniformly rather than case by case: a 'NonEmpty' collection is used
-- exactly where the collection's own emptiness would falsify the
-- constructor's claim to exist at all — a list ('BulletList') with no
-- items is not a sparse list, it is not a list; a table with no rows,
-- or a row with no cells, is not sparse data, it is not a table or a
-- row. Everywhere else — a paragraph's text, a heading's text, a table
-- cell's content, a list item's blocks, the content of an emphasis or
-- link span — a plain, possibly-empty list is used, because the slot
-- itself is already established by its constructor; whether it happens
-- to hold visible content is a mundane content fact, not a structural
-- one. An empty paragraph and an empty spreadsheet cell are both
-- ordinary data. Forcing either into 'NonEmpty' does not make the
-- format safer, it makes ordinary, common documents unrepresentable —
-- which is the same defect as the one 'NonEmpty' exists to prevent,
-- pointed the other way.
module Mirror.Document
  ( -- Types
    Document (..)
  , Block (..)
  , Inline (..)
  , ListKind (..)
  , ListItem (..)
  , Cell
  , TableRow (..)
  , Table                 -- type only — constructor NOT exported
  , HeadingLevel (..)
  , LanguageTag (..)
  , Url
  , unUrl
  , NonEmptyText
  , unNonEmptyText
    -- Smart constructors (the only way to build the refined types above)
  , mkNonEmptyText
  , mkUrl
  , mkImageUrl
  , mkDocument
  , mkTable
  , headingLevelFromInt
  , listKindFromText
  , languageTagFromText
  , note
    -- Read-only accessors for the opaque 'Table' type
  , tableHeader
  , tableBody
  , tableColumnCount
  ) where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Text.URI (mkURI)
import qualified Text.URI as URI

import Mirror.Error

-- | Exactly six inhabitants. Nothing to validate at runtime because
-- there is nothing illegal to construct.
data HeadingLevel = H1 | H2 | H3 | H4 | H5 | H6
  deriving (Eq, Ord, Show, Enum, Bounded)

data ListKind = Ordered | Unordered
  deriving (Eq, Show)

-- | A closed set of fenced-code-block languages Mirror knows how to
-- attribute a CSS class to. A language outside this set is a reported
-- 'InvalidLanguageTag', not a silently-accepted arbitrary string —
-- there is no free-text escape hatch alongside the enumeration.
data LanguageTag
  = LangHaskell | LangJavaScript | LangPython | LangJson | LangShell
  | LangSql | LangYaml | LangMarkdown | LangText
  deriving (Eq, Show, Enum, Bounded)

-- | Required-non-empty text. Used for 'documentTitle' only.
newtype NonEmptyText = UnsafeNonEmptyText Text
  deriving (Eq, Ord, Show)

unNonEmptyText :: NonEmptyText -> Text
unNonEmptyText (UnsafeNonEmptyText t) = t

-- | A syntactically valid URL. Constructed only via 'mkUrl'.
newtype Url = UnsafeUrl Text
  deriving (Eq, Ord, Show)

unUrl :: Url -> Text
unUrl (UnsafeUrl t) = t

-- | A parsed, but not yet rendered, document. All three parsers (§6-§8)
-- funnel into this same type through 'mkDocument', so validation is
-- written once and enforced identically regardless of source format.
data Document = Document
  { documentTitle  :: Maybe NonEmptyText
  , documentBlocks :: NonEmpty Block
  } deriving (Eq, Show)

data Block
  = Heading HeadingLevel [Inline]
  | Paragraph [Inline]
  | BulletList ListKind (NonEmpty ListItem)
  | TableBlock Table
  | BlockQuote (NonEmpty Block)
  | CodeBlock (Maybe LanguageTag) Text
  | Image Url Text (Maybe (Int, Int))
    -- ^ Alt text is plain 'Text'. An empty alt attribute is not an
    -- error — it is the standard, correct HTML for a decorative image.
    -- The pixel dimensions are 'Just' only when the source format
    -- states them explicitly (DOCX's @wp:extent@); 'Nothing' for
    -- Markdown\/JSON, where no dimension is knowable without fetching
    -- the asset — a stated, narrow limitation rather than a silent
    -- gap. The renderer (§9) emits @width@\/@height@ attributes only
    -- when this is 'Just', to avoid Cumulative Layout Shift wherever
    -- the dimension is actually known.
  | HorizontalRule
  deriving (Eq, Show)

-- | One list item's content. May legitimately be empty — see the
-- module-level note on 'NonEmpty' versus plain lists. With no
-- invariant left to protect, the constructor is exported directly.
newtype ListItem = ListItem [Block]
  deriving (Eq, Show)

-- | The content of one table cell, prior to any cross-row validation.
-- May legitimately be empty — a blank spreadsheet-style cell or an
-- empty DOCX table cell is data, not an error.
type Cell = [Inline]

-- | A single row's cells. No hidden invariant beyond non-emptiness of
-- the row itself, which 'NonEmpty' already guarantees intrinsically —
-- so, unlike 'Table', this constructor is safe to export directly.
newtype TableRow = TableRow (NonEmpty Cell)
  deriving (Eq, Show)

-- | Opaque. The invariant "every row, header included, has the same
-- number of cells" is a *cross-row* property no amount of
-- pattern-matching on 'TableRow' alone can see, so it can only be
-- protected by refusing to export a constructor that could build an
-- inconsistent 'Table'. 'mkTable' is the only way to obtain one.
data Table = UnsafeTable (Maybe TableRow) (NonEmpty TableRow)
  deriving (Eq, Show)

tableHeader :: Table -> Maybe TableRow
tableHeader (UnsafeTable h _) = h

tableBody :: Table -> NonEmpty TableRow
tableBody (UnsafeTable _ b) = b

-- | Exercised directly by rectangularity property tests (§12).
tableColumnCount :: Table -> Int
tableColumnCount (UnsafeTable _ (TableRow cells :| _)) = length cells

data Inline
  = Plain Text
    -- ^ Named 'Plain', not 'Text', deliberately: this constructor
    -- lives in a module that also imports and pervasively uses
    -- @Data.Text@'s 'Text' /type/. Naming the constructor identically
    -- to that type means "Text" refers to two unrelated things
    -- depending on term\/type context at every call site — exactly
    -- the ambiguity §4.8 of the Haskell standard warns against.
  | Emphasis [Inline]
  | Strong [Inline]
  | Link Url (NonEmpty Inline)
    -- ^ Unlike a paragraph's or table cell's inline content, a link's
    -- visible text is not "ordinary, possibly-empty data": an anchor
    -- with nothing to anchor is both inaccessible (no accessible name
    -- for assistive technology) and unusable (nothing to click). The
    -- type itself rules this out, the same way 'NonEmpty' already
    -- rules out a list or table with no rows — see 'Document.Raw.toInline'
    -- for where an empty raw link is rejected as 'EmptyLinkText'.
  | InlineCode Text
  | SoftBreak
  deriving (Eq, Show)

-- Smart constructors ---------------------------------------------------

-- | Attaches an error to the 'Nothing' case of a 'Maybe', turning any
-- total, 'Maybe'-returning check into an 'Either MirrorError'. Shared
-- by every smart constructor below and by the parsers (§6-§8), so a
-- validation check is always phrased the same way regardless of
-- which module is holding the 'Maybe'.
note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

mkNonEmptyText :: Text -> Either MirrorError NonEmptyText
mkNonEmptyText t
  | Text.null (Text.strip t) = Left (ErrValidation EmptyDocumentTitle)
  | otherwise                 = Right (UnsafeNonEmptyText t)

-- | Closed, deliberately short allow-lists of URL schemes Mirror will
-- ever emit into an @href@ or @src@ attribute. This is a security
-- boundary, not a style preference: a syntactically valid URI with a
-- scheme like @javascript:@ or @vbscript:@ is exactly as well-formed
-- as an @https:@ one to 'mkURI', but executes attacker-supplied
-- content the instant a reader clicks it or loads it — and Mirror's
-- three input formats (a Markdown file, a JSON document, a @.docx@
-- someone emailed you) are all plausibly untrusted.
--
-- Links and images are given /different/ allow-lists, deliberately,
-- rather than one shared list: an embedded DOCX image is legitimately
-- a @data:@ URI (§7 — images are embedded as base64 data URIs), but a
-- clickable @<a href="data:text/html,...">@ is its own, separate
-- script-injection surface most browsers will still navigate to, so
-- @data:@ is allowed for 'mkImageUrl' and refused for 'mkUrl'. A
-- scheme outside the relevant set is 'DisallowedUrlScheme', never
-- silently accepted by either.
allowedLinkUrlSchemes :: [Text]
allowedLinkUrlSchemes = ["http", "https", "mailto"]

allowedImageUrlSchemes :: [Text]
allowedImageUrlSchemes = ["http", "https", "data"]

-- | Shared implementation: delegates syntactic validation to
-- @modern-uri@'s 'mkURI', which is total (a 'Maybe', never a partial
-- parse), so there is no hand-rolled URL grammar to get wrong — then
-- additionally restricts the result to the given allow-list. A
-- scheme-less (relative) reference has no 'URI.uriScheme' at all and
-- is accepted as-is, since a relative link or image path cannot
-- itself carry an executable scheme.
mkUrlWith :: [Text] -> Text -> Either MirrorError Url
mkUrlWith allowed t = do
  uri <- note (ErrValidation (InvalidUrl t)) (mkURI t)
  case URI.unRText <$> URI.uriScheme uri of
    Nothing -> Right (UnsafeUrl t)
    Just scheme
      | Text.toLower scheme `elem` allowed -> Right (UnsafeUrl t)
      | otherwise -> Left (ErrValidation (DisallowedUrlScheme scheme))

-- | For a hyperlink's destination ('Mirror.Document.Link').
mkUrl :: Text -> Either MirrorError Url
mkUrl = mkUrlWith allowedLinkUrlSchemes

-- | For an image's @src@ ('Mirror.Document.Image'). Permits @data:@
-- in addition to 'mkUrl''s allow-list, for embedded DOCX images.
mkImageUrl :: Text -> Either MirrorError Url
mkImageUrl = mkUrlWith allowedImageUrlSchemes

mkDocument :: Maybe NonEmptyText -> [Block] -> Either MirrorError Document
mkDocument title blocks =
  Document title <$> note (ErrValidation EmptyDocumentBody) (nonEmpty blocks)

-- | The only way to obtain a 'Table'. Every row — the optional header
-- and every body row — is checked against the first row's width via
-- 'NE.filter'; on mismatch the first offending row's width is
-- reported. 'allRows' is 'NonEmpty' throughout (built with 'NE.cons',
-- never degraded to a plain list), so 'NE.head' here is the total
-- field accessor on a non-empty-by-construction type, not
-- 'Prelude.head'. A missing header is not an error (many tables have
-- none); zero body rows and a body row with zero cells are two
-- distinct, distinctly-reported errors, not one conflated case.
mkTable :: Maybe [Cell] -> [[Cell]] -> Either MirrorError Block
mkTable headerCells bodyCellRows = do
  header     <- traverse toRow headerCells
  bodyRowsNE <- note (ErrValidation EmptyTable) (nonEmpty bodyCellRows)
  body       <- traverse toRow bodyRowsNE
  let allRows = maybe body (`NE.cons` body) header
      width   = rowWidth (NE.head allRows)
  case NE.filter ((/= width) . rowWidth) allRows of
    []      -> Right (TableBlock (UnsafeTable header body))
    (r : _) -> Left (ErrValidation (MismatchedTableColumns width (rowWidth r)))
  where
    toRow cells = TableRow <$> note (ErrValidation EmptyTableRow) (nonEmpty cells)
    rowWidth (TableRow cells) = length cells

-- | Total map from a bare integer heading level onto 'HeadingLevel'.
-- The single conversion point for both parsers that present a heading
-- level as a number — ATX '#' counting in Markdown (§6) and a
-- @"level"@ field in JSON (§8) — so the 1..6 range is enforced once,
-- not hand-checked twice.
headingLevelFromInt :: Int -> Maybe HeadingLevel
headingLevelFromInt = \case
  1 -> Just H1; 2 -> Just H2; 3 -> Just H3
  4 -> Just H4; 5 -> Just H5; 6 -> Just H6
  _ -> Nothing

-- | Total map from JSON's @"kind"@ field onto 'ListKind'. The set of
-- accepted spellings is closed and exact — no case-insensitivity, no
-- synonyms — so that a typo is a reported schema violation rather than
-- a silently-guessed default.
listKindFromText :: Text -> Maybe ListKind
listKindFromText = \case
  "ordered"   -> Just Ordered
  "unordered" -> Just Unordered
  _           -> Nothing

-- | Total map from a fenced-code-block language name onto 'LanguageTag'.
languageTagFromText :: Text -> Maybe LanguageTag
languageTagFromText = \case
  "haskell"    -> Just LangHaskell
  "javascript" -> Just LangJavaScript
  "python"     -> Just LangPython
  "json"       -> Just LangJson
  "shell"      -> Just LangShell
  "sql"        -> Just LangSql
  "yaml"       -> Just LangYaml
  "markdown"   -> Just LangMarkdown
  "text"       -> Just LangText
  _            -> Nothing
