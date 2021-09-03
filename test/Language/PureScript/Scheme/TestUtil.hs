module Language.PureScript.Scheme.TestUtil where

import Data.Text (Text)
import Turtle ((</>))

import qualified Turtle as Turtle
import qualified Language.PureScript.Scheme.IOUtil as IOUtil

inDir :: Turtle.MonadIO m => Turtle.FilePath -> m b -> m b
inDir directory do_ = do
  oldDir <- Turtle.pwd
  Turtle.cd directory
  res <- do_
  Turtle.cd oldDir
  return res

shellInDir
  :: Turtle.MonadIO m
  => Turtle.FilePath
  -> Text
  -> m (Turtle.ExitCode, Turtle.Text, Turtle.Text)
shellInDir dir cmd = inDir dir (Turtle.shellStrictWithErr cmd mempty)

basePath :: Turtle.FilePath
basePath = "test" </> "resources"

purescriptPath :: Turtle.FilePath
purescriptPath = basePath </> "purescript"

outputPath :: Turtle.FilePath
outputPath = purescriptPath </> "output"

schemePath :: Turtle.FilePath
schemePath = basePath </> "scheme"

spagoBuild :: IO ()
spagoBuild = do
  (exitCode, _out, err) <- shellInDir purescriptPath "spago build"
  case exitCode of
    Turtle.ExitSuccess -> pure ()
    Turtle.ExitFailure _ -> error $ "spago error: " <> show err

findSchemeFixtures :: IO [Text]
findSchemeFixtures = IOUtil.findEnds (toText schemePath) "lib.sls"

schemeOutput :: Text -> Text
schemeOutput path =
  let
    file = case Turtle.stripPrefix (schemePath </> "") (Turtle.fromText path) of
      Just p -> toText p
      Nothing -> path
  in toText (outputPath </> Turtle.fromText file)

toText :: Turtle.FilePath -> Text
toText f = case Turtle.toText f of
  Left _ -> error "Could not convert FilePath to Text"
  Right t -> t
