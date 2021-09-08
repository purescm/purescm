module Run where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.IO as IO
import qualified System.Process as Process

run :: Text -> Text -> IO ()
run outputPath moduleName = do
  let args = [ "--quiet"
             , "--libdirs", outputPath
             ]

  let includeCommand
        = "(import (prefix (" <> moduleName <> " lib) " <> moduleName <> ".))\n"
  let runMainCommand
        = "(" <> moduleName <> ".main)\n"
  let exitCommand = "(exit)\n"

  let proc = (Process.proc (Text.unpack "scheme")
                           (map Text.unpack args))
             { Process.std_in = Process.CreatePipe
             , Process.delegate_ctlc = True
             }

  (Just stdIn, _stdOut, _stdErr, handle) <- Process.createProcess proc

  IO.hPutStr stdIn (Text.unpack includeCommand)
  IO.hPutStr stdIn (Text.unpack runMainCommand)
  IO.hPutStr stdIn exitCommand

  _exitCode <- Process.waitForProcess handle

  return ()
