module Main where

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

rmIf :: String -> Aff Unit
rmIf p = liftEffect do
  c <- FS.exists p
  when c $
    FS.rm' p { force: true, maxRetries: 1, recursive: true, retryDelay: 1 }

fetchChezSrfi :: String -> ExceptT String Aff Unit
fetchChezSrfi clonePath = ExceptT do
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

vendorChezSrfi :: String -> String -> ExceptT String Aff Unit
vendorChezSrfi clonePath vendorPath = ExceptT do
  Console.log "Vendoring chez-srfi..."
  rmIf vendorPath
  spawned <- Execa.execa "./install.chezscheme.sps" [ vendorPath ] (_ { cwd = Just clonePath })
  bimap (_.message) (const unit) <$> spawned.result

cleanChezSrfi :: String -> ExceptT String Aff Unit
cleanChezSrfi clonePath = ExceptT do
  Console.log "Cleaning chez-srfi..."
  rmIf clonePath
  pure (Right unit)

main :: Effect Unit
main = launchAff_ do
  spawned <- runExceptT do
    fetchChezSrfi "../chez-srfi"
    vendorChezSrfi "../chez-srfi" "../vendor"
    cleanChezSrfi "../chez-srfi"
  case spawned of
    Left message ->
      Console.error $ "Failed: " <> message
    Right _ ->
      Console.log $ "Finished..."
