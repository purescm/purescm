module Tools.Main where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.FS.Sync as FS
import Node.Library.Execa as Execa
import Node.Path (FilePath)
import Node.Path as Path

clonePath :: FilePath
clonePath = "chez-srfi"

vendorPath :: FilePath
vendorPath = "vendor"

revision :: String
revision = "e056421"

rmIf :: String -> Aff Unit
rmIf p = liftEffect do
  c <- FS.exists p
  when c $
    FS.rm' p { force: true, maxRetries: 1, recursive: true, retryDelay: 1 }

fetchChezSrfi :: ExceptT String Aff Unit
fetchChezSrfi = ExceptT do
  cloneExists <- liftEffect $ FS.exists clonePath
  cloneOrFetch <- case cloneExists of
    true -> do
      Console.log "Fetching chez-srfi..."
      spawned <- Execa.execa "git" [ "fetch", "origin" ] (_ { cwd = Just clonePath })
      spawned.result
    false -> do
      Console.log "Cloning chez-srfi..."
      spawned <- Execa.execa "git"
        [ "clone", "https://github.com/arcfide/chez-srfi.git", clonePath ]
        identity
      spawned.result
  pure $ bimap (_.message) (const unit) cloneOrFetch

checkoutRevision :: ExceptT String Aff Unit
checkoutRevision = ExceptT do
  Console.log $ "Checking out " <> revision <> "..."
  spawned <- Execa.execa "git" [ "checkout", revision ] (_ { cwd = Just clonePath })
  bimap (_.message) (const unit) <$> spawned.result

vendorChezSrfi :: ExceptT String Aff Unit
vendorChezSrfi = ExceptT do
  Console.log "Vendoring chez-srfi..."
  rmIf vendorPath
  -- vendorPath is a sibling of clonePath
  spawned <- Execa.execa "./install.chezscheme.sps" [ Path.concat [ "..", vendorPath ] ]
    (_ { cwd = Just clonePath })
  bimap (_.message) (const unit) <$> spawned.result

cleanChezSrfi :: ExceptT String Aff Unit
cleanChezSrfi = ExceptT do
  Console.log "Cleaning chez-srfi..."
  rmIf clonePath
  pure (Right unit)

main :: Effect Unit
main = launchAff_ do
  spawned <- runExceptT do
    fetchChezSrfi
    checkoutRevision
    vendorChezSrfi
    cleanChezSrfi
  case spawned of
    Left message ->
      Console.error $ "Failed: " <> message
    Right _ ->
      Console.log $ "Finished..."
