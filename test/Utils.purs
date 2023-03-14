module Test.Utils where

import Prelude

import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Parallel (parTraverse)
import Data.Argonaut as Json
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Lazy as Lazy
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Posix.Signal (Signal(..))
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, effectCanceler, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, freeze)
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ExecResult, Exit(..), defaultExecOptions, defaultSpawnOptions, inherit)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms (mkPerms)
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Process as Process
import Node.Stream as Stream
import PureScript.Backend.Optimizer.CoreFn (Ann, Module, ModuleName(..))
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)
import PureScript.Backend.Optimizer.CoreFn.Sort (emptyPull, pullResult, resumePull, sortModules)

spawnFromParent :: String -> Array String -> Aff Unit
spawnFromParent command args = makeAff \k -> do
  childProc <- ChildProcess.spawn command args defaultSpawnOptions { stdio = inherit }
  ChildProcess.onExit childProc case _ of
    Normally code
      | code > 0 -> Process.exit code
      | otherwise -> k (Right unit)
    BySignal _ ->
      Process.exit 1
  pure $ effectCanceler do
    ChildProcess.kill SIGABRT childProc

execWithStdin :: String -> String -> Aff ExecResult
execWithStdin command input = makeAff \k -> do
  childProc <- ChildProcess.exec command defaultExecOptions (k <<< pure)
  _ <- Stream.writeString (ChildProcess.stdin childProc) UTF8 input mempty
  Stream.end (ChildProcess.stdin childProc) mempty
  pure $ effectCanceler $ ChildProcess.kill SIGABRT childProc

bufferToUTF8 :: Buffer -> Aff String
bufferToUTF8 = liftEffect <<< map (ImmutableBuffer.toString UTF8) <<< freeze

mkdirp :: FilePath -> Aff Unit
mkdirp path = FS.mkdir' path { recursive: true, mode: mkPerms Perms.all Perms.all Perms.all }

foreign import loadModuleMainImpl
  :: (Error -> Effect Unit)
  -> (Effect Unit -> Effect Unit)
  -> Effect Unit
  -> FilePath
  -> Effect Unit

-- | Note: this code needs to be updated to run Chez Scheme scripts
loadModuleMain :: FilePath -> Aff (Maybe (Effect Unit))
loadModuleMain path = makeAff \k -> do
  loadModuleMainImpl (k <<< Left) (k <<< Right <<< Just) (k (Right Nothing)) path
  mempty

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  makeAff \k -> do
    src <- createReadStream from
    dst <- createWriteStream to
    res <- Stream.pipe src dst
    Stream.onError src (k <<< Left)
    Stream.onError dst (k <<< Left)
    Stream.onError res (k <<< Left)
    Stream.onFinish res (k (Right unit))
    pure $ effectCanceler do
      Stream.destroy res
      Stream.destroy dst
      Stream.destroy src

coreFnModulesFromOutput
  :: String
  -> NonEmptyArray String
  -> Aff (Either (NonEmptyArray (Tuple FilePath String)) (List (Module Ann)))
coreFnModulesFromOutput path globs = runExceptT do
  paths <- Set.toUnfoldable <$> lift
    (expandGlobs path ((_ <> "/corefn.json") <$> NonEmptyArray.toArray globs))
  case NonEmptyArray.toArray globs of
    [ "**" ] ->
      sortModules <$> modulesFromPaths paths
    _ ->
      go <<< foldl resumePull emptyPull =<< modulesFromPaths paths
  where
  modulesFromPaths paths = ExceptT do
    { left, right } <- separate <$> parTraverse readCoreFnModule paths
    pure $ maybe (Right right) Left $ NonEmptyArray.fromArray left

  pathFromModuleName (ModuleName mn) =
    path <> "/" <> mn <> "/corefn.json"

  go pull = case pullResult pull of
    Left needed ->
      go <<< foldl resumePull pull =<< modulesFromPaths
        (pathFromModuleName <$> NonEmptySet.toUnfoldable needed)
    Right modules ->
      pure $ Lazy.force modules

readCoreFnModule :: String -> Aff (Either (Tuple FilePath String) (Module Ann))
readCoreFnModule filePath = do
  contents <- FS.readTextFile UTF8 filePath
  case lmap Json.printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
    Left err -> do
      pure $ Left $ Tuple filePath err
    Right mod ->
      pure $ Right mod
