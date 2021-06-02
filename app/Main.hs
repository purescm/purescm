module Main where

import           Data.Foldable                   (for_)
import qualified Data.Text.IO                    as Text.IO
import qualified Turtle                          as Turtle
import           Language.PureScript.Scheme.Make (compile)
import           TestUtil                        (corefnFile, schemeFile, toText,
                                                  findModules, removeSchemeFiles,
                                                  removeCorefnFiles, buildCorefn)


main :: IO ()
main = do
  removeSchemeFiles
  removeCorefnFiles
  buildCorefn
  modules <- findModules
  for_ modules $ \m -> do
    let corefn = corefnFile m
    let schemePath = schemeFile m
    scheme <- compile $ Turtle.encodeString corefn
    Text.IO.putStrLn $ "Compiling " <> m <> " to " <> toText schemePath
    Turtle.liftIO $ Turtle.writeTextFile schemePath scheme
