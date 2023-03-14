module Options where

import Data.Text (Text)
import qualified Options.Applicative as Opts
import Options.Applicative (Parser)

data Options = Options
  { optionOutput :: Text
  , optionRunFunction :: Maybe Text
  }

parser :: Parser Options
parser = Options
     <$> outputOption
     <*> runFunctionOption

outputOption :: Parser Text
outputOption = Opts.strOption
             $ Opts.long "output"
            <> Opts.help "Output directory"
            <> Opts.metavar "PATH"
            <> Opts.value "output"

runFunctionOption :: Parser (Maybe Text)
runFunctionOption = Opts.optional
                  $ Opts.strOption
                  $ Opts.long "run"
                 <> Opts.help "Run a fully qualified function"
                 <> Opts.metavar "FUNCTION"

runParser :: [String] -> IO Options
runParser = Opts.handleParseResult . execParserPure opts
  where
    opts :: Opts.ParserInfo Options
    opts = Opts.info parser description

    description :: Opts.InfoMod Options
    description = Opts.fullDesc <> headerDesc

    headerDesc :: Opts.InfoMod Options
    headerDesc = Opts.progDesc "purescm"

    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args
