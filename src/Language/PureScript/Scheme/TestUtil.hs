module Language.PureScript.Scheme.TestUtil where

import           Data.Maybe                      (catMaybes)
import qualified Data.Text                       as Text
import           Data.Text                       (Text)
import qualified Control.Foldl                   as Fold
import qualified Turtle                          as Turtle
import           Turtle                          ((</>), (<.>))

basePath :: Turtle.FilePath
basePath = "test" </> "resources"

purescriptPath :: Turtle.FilePath
purescriptPath = basePath </> "purescript"

sourcePath :: Turtle.FilePath
sourcePath = purescriptPath </> "src"

corefnPath :: Turtle.FilePath
corefnPath = purescriptPath </> "output"

schemePath :: Turtle.FilePath
schemePath = basePath </> "scheme"

buildCmd :: Text
buildCmd = "spago build -u '-g corefn'"

corefnFile :: Text -> Turtle.FilePath
corefnFile module_ = corefnPath </> Turtle.fromText module_ </> "corefn.json"

schemeFile :: Text -> Turtle.FilePath
schemeFile module_ = schemePath </> Turtle.fromText module_ <.> "sls"

toText :: Turtle.FilePath -> Text
toText f = case Turtle.toText f of
  Left _ -> error "Could not convert FilePath to Text"
  Right t -> t

moduleName :: Turtle.FilePath -> Text
moduleName f = Text.intercalate "."
             $ Text.splitOn "/"
             $ toText
             $ Turtle.dropExtension f

findSourceFiles :: IO [Turtle.FilePath]
findSourceFiles = do
  paths <- Turtle.fold (Turtle.find (Turtle.ends ".purs") sourcePath) Fold.list
  let paths' = fmap (Turtle.stripPrefix (sourcePath </> "")) paths
  return $ catMaybes paths'

findModules :: IO [Text]
findModules = do
  files <- findSourceFiles
  return $ fmap moduleName files

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

buildCorefn :: IO ()
buildCorefn = do
  (exitCode, _out, err) <- shellInDir purescriptPath buildCmd
  case exitCode of
    Turtle.ExitSuccess -> pure ()
    Turtle.ExitFailure _ -> error $ "spago error: " <> show err
