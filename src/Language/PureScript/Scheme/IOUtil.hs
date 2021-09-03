module Language.PureScript.Scheme.IOUtil where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Control.Exception as Exception
import qualified Control.Foldl as Fold
import qualified Turtle as Turtle
import qualified System.Directory as Directory
import qualified System.Directory.Internal.Prelude as Directory.Internal

filePathToText :: Turtle.FilePath -> Text
filePathToText path = case Turtle.toText path of
  Left  _ -> error "Could not convert FilePath to Text"
  Right t -> t

find' :: Turtle.FilePath -> Turtle.Pattern a -> IO [Turtle.FilePath]
find' basePath pattern = Turtle.fold (Turtle.find pattern basePath) Fold.list

find :: Text -> Turtle.Pattern a -> IO [Text]
find basePath pattern = do
  paths <- find' (Turtle.fromText basePath) pattern
  return $ fmap filePathToText paths

findEnds :: Text -> Turtle.Pattern Text -> IO [Text]
findEnds basePath endsWith = find basePath (Turtle.ends endsWith)

readJSONFile
  :: Aeson.Types.FromJSON a
  => Text
  -> (a -> Aeson.Types.Parser b)
  -> IO b
readJSONFile path fromJSON = do
  let path' = Text.unpack path
  json <- Aeson.eitherDecodeFileStrict' path'
  let result = Aeson.Types.parseEither fromJSON =<< json
  case result of
    Left err -> error err
    Right x -> return x

error' :: Text -> a
error' text = error $ Text.unpack text

-- | Create a directory. If it exists do nothing.
mkdir :: Text -> IO ()
mkdir path = do
  result <- Exception.try $ Directory.createDirectory (Text.unpack path)
  case result of
    Left e
      | Directory.Internal.isDoesNotExistError e -> return ()
      | otherwise -> ioError e
    Right _ -> return ()

writeTextFile :: Text -> Text -> IO ()
writeTextFile path text
  = Turtle.liftIO
  $ Turtle.writeTextFile (Turtle.fromText path) text

print' :: Text -> IO ()
print' text = Text.IO.putStrLn text

changeFileExtension :: Text -> Text -> Text
changeFileExtension path newExtension
  = filePathToText
  $ Turtle.dropExtension (Turtle.fromText path) Turtle.<.> newExtension

cp :: Text -> Text -> IO ()
cp from to = Turtle.cp (Turtle.fromText from) (Turtle.fromText to)
