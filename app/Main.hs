-- | The CLI entry point.
--
-- There is no intermediate record shuttling parsed arguments to
-- 'mkPipelineConfig' — 'mkPipelineConfig' itself is the applicative
-- target the command-line parser builds. A named record here would
-- exist purely to be immediately pattern-matched apart again, and a
-- record whose fields are only ever destructured together, never
-- accessed individually, is a shape smart constructors intentionally
-- deprecate.
module Main (main) where

import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure)

import Mirror.Error (MirrorError, renderMirrorError)
import Mirror.Pipeline

main :: IO ()
main = do
  configOrErr <- execParser cliParser
  case configOrErr of
    Left err  -> reportAndExit err
    Right cfg -> run cfg >>= either reportAndExit (const (pure ()))
  where
    reportAndExit err = TIO.putStrLn (renderMirrorError err) >> exitFailure

-- | Builds an @Either MirrorError PipelineConfig@ directly: CLI parsing
-- and the resulting configuration's own validation (§10) are two
-- different concerns, and the type says so plainly rather than
-- assuming the arguments were already good by the time they arrive.
cliParser :: ParserInfo (Either MirrorError PipelineConfig)
cliParser = info (helper <*> argsP) (fullDesc <> progDesc "Mirror: document transformer")
  where
    argsP = mkPipelineConfig
      <$> strArgument (metavar "INPUT")
      <*> optional (strOption (long "css" <> metavar "FILE"
                                <> help "stylesheet to embed instead of Mirror's default"))
      <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE"
                                <> help "output path (default: INPUT with a .html extension)"))
