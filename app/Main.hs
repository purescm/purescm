module Main where

import System.Environment (getArgs)
import Options (Options(..))
import Build (build)
import Run (run)
import Language.PureScript.Scheme.IOUtil (print', tshow)

import qualified System.IO as IO
import qualified Options as Options

main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  Options { optionOutput, optionRunFunction } <- Options.runParser =<< getArgs
  print' $ "Options: " <> optionOutput
  print' $ "    output: " <> optionOutput
  print' $ "    runFunction: " <> tshow optionRunFunction
  print' ""

  build optionOutput

  case optionRunFunction of
    Nothing -> pure ()
    Just moduleAndFunction -> run optionOutput moduleAndFunction
