-- | Orchestrates one end-to-end conversion: read bytes, parse by
-- format, render, write HTML and a sibling stylesheet.
--
-- The extensibility mechanism, stated precisely rather than left as a
-- promise: 'SourceFormat' is a closed enumeration, and every function
-- that branches on it — 'detectFormat' and 'parseByFormat' — is a
-- total pattern match checked exhaustive by @-Wincomplete-patterns
-- -Werror@ (§0, rule 6). Adding a fourth source format is
-- mechanical and the compiler enforces every step: add a constructor
-- to 'SourceFormat', map its extension in 'detectFormat', add one
-- equation to 'parseByFormat' calling a new parser shaped like the
-- three below. Skip any step and the build fails on a
-- non-exhaustive-pattern error at the exact call site that needed
-- updating — not a runtime surprise on someone else's input. §13 works
-- through this precisely for a hypothetical fourth format.
module Mirror.Pipeline
  ( PipelineConfig
  , SourceFormat (..)
  , mkPipelineConfig
  , run
  ) where

import Control.Exception (IOException, try)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.FilePath (replaceExtension, takeExtension, takeFileName)
import System.IO.Error (isDoesNotExistError)

import Mirror.Css (defaultStylesheet)
import Mirror.Document (Document)
import Mirror.Error
import Mirror.Parser.Docx (parseDocx)
import Mirror.Parser.Json (parseJson)
import Mirror.Parser.Markdown (parseMarkdown)
import Mirror.Renderer.Html (renderDocument)

data SourceFormat = FormatMarkdown | FormatDocx | FormatJson
  deriving (Eq, Show)

-- | Opaque: the only way to obtain one is 'mkPipelineConfig', which
-- resolves the source format from the file extension up front. There
-- is no default branch that falls back to a guessed format for an
-- unrecognised extension.
data PipelineConfig = PipelineConfig
  { cfgInputPath      :: FilePath
  , cfgInputFormat    :: SourceFormat
  , cfgOutputPath     :: FilePath
  , cfgStylesheetPath :: Maybe FilePath
  } deriving (Eq, Show)

mkPipelineConfig :: FilePath -> Maybe FilePath -> Maybe FilePath -> Either MirrorError PipelineConfig
mkPipelineConfig input cssPath outPath = do
  fmt <- detectFormat input
  pure PipelineConfig
    { cfgInputPath      = input
    , cfgInputFormat    = fmt
    , cfgOutputPath     = maybe (replaceExtension input ".html") id outPath
    , cfgStylesheetPath = cssPath
    }

-- | Total over the extensions Mirror supports; anything else is a
-- named, reported error rather than a silent guess.
detectFormat :: FilePath -> Either MirrorError SourceFormat
detectFormat path = case takeExtension path of
  ".md"       -> Right FormatMarkdown
  ".markdown" -> Right FormatMarkdown
  ".docx"     -> Right FormatDocx
  ".json"     -> Right FormatJson
  ext         -> Left (ErrParse (UnsupportedSourceExtension path (Text.pack ext)))

-- | Both the HTML output and the resolved stylesheet are writes to
-- paths Mirror itself derived, so both failures are reported as
-- 'OutputFileUnwritable' — 'StylesheetUnreadable' is reserved for the
-- distinct failure mode of reading a *user-supplied* CSS file in
-- 'resolveStylesheet'.
run :: PipelineConfig -> IO (Either MirrorError ())
run cfg = runExceptT $ do
  rawBytes <- readSourceBytes (cfgInputPath cfg)
  doc      <- parseByFormat (cfgInputFormat cfg) (cfgInputPath cfg) rawBytes
  css      <- resolveStylesheet (cfgStylesheetPath cfg)
  let cssPath = replaceExtension (cfgOutputPath cfg) ".css"
  html <- except (renderDocument (Text.pack (takeFileName cssPath)) doc)
  writeTextFile OutputFileUnwritable (cfgOutputPath cfg) html
  writeTextFile OutputFileUnwritable cssPath css

-- | The one place all three parsers meet. Exhaustive over
-- 'SourceFormat' — see the module note.
parseByFormat :: SourceFormat -> FilePath -> BSL.ByteString -> ExceptT MirrorError IO Document
parseByFormat FormatMarkdown _path bytes = do
  txt <- except (decodeStrictUtf8 bytes)
  except (parseMarkdown txt)
parseByFormat FormatDocx path bytes = parseDocx path bytes
parseByFormat FormatJson _path bytes = except (parseJson bytes)

-- | Surfaces invalid encoding as a named error rather than silently
-- substituting replacement characters. Only Markdown needs this
-- pre-step: DOCX is a zip of XML, and JSON's own decoder handles
-- encoding as part of general syntax checking (§8) — neither hands
-- Mirror a bare 'Text' value the way the Markdown grammar (§6) does.
decodeStrictUtf8 :: BSL.ByteString -> Either MirrorError Text
decodeStrictUtf8 bytes = case TLE.decodeUtf8' bytes of
  Left unicodeErr -> Left (ErrParse (MarkdownInvalidEncoding (Text.pack (show unicodeErr))))
  Right lazyText  -> Right (TL.toStrict lazyText)

-- | Distinguishes "the file is not there" from every other read
-- failure via 'isDoesNotExistError', so 'SourceFileNotFound' is the
-- diagnostic a missing path actually gets, not a generic one that
-- happens to mention "does not exist" only because the underlying
-- 'IOException''s own 'Show' instance says so.
readSourceBytes :: FilePath -> ExceptT MirrorError IO BSL.ByteString
readSourceBytes path = ExceptT $ do
  outcome <- try (BSL.readFile path)
  pure $ case (outcome :: Either IOException BSL.ByteString) of
    Left ioErr
      | isDoesNotExistError ioErr -> Left (ErrIO (SourceFileNotFound path))
      | otherwise                 -> Left (ErrIO (SourceFileUnreadable path (Text.pack (show ioErr))))
    Right bytes -> Right bytes

-- | 'Nothing' resolves to the compile-time-embedded default from
-- "Mirror.Css"; 'Just path' reads the user's file unchanged, reporting
-- a named error rather than silently falling back to the default if
-- it can't be read.
resolveStylesheet :: Maybe FilePath -> ExceptT MirrorError IO Text
resolveStylesheet Nothing = pure defaultStylesheet
resolveStylesheet (Just path) = ExceptT $ do
  outcome <- try (TIO.readFile path)
  pure $ either (Left . ErrIO . StylesheetUnreadable path . Text.pack . show) Right
               (outcome :: Either IOException Text)

writeTextFile :: (FilePath -> Text -> IOFailure) -> FilePath -> Text -> ExceptT MirrorError IO ()
writeTextFile mkErr path content = ExceptT $ do
  outcome <- try (TIO.writeFile path content)
  pure $ either (Left . ErrIO . mkErr path . Text.pack . show) Right
               (outcome :: Either IOException ())
