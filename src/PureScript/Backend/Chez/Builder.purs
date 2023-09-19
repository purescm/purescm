module PureScript.Backend.Chez.Builder where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Parallel (parTraverse)
import Data.Argonaut as Json
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Lazy as Lazy
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Process as Process
import PureScript.Backend.Optimizer.Builder (BuildEnv, buildModules)
import PureScript.Backend.Optimizer.Convert (BackendModule, OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module, ModuleName(..))
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)
import PureScript.Backend.Optimizer.CoreFn.Sort (emptyPull, pullResult, resumePull, sortModules)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics (InlineDirectiveMap)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)
import PureScript.CST.Errors (printParseError)

basicBuildMain
  :: { coreFnDirectory :: FilePath
     , coreFnGlobs :: NonEmptyArray String
     , externalDirectivesFile :: Maybe FilePath
     , onCodegenModule :: BuildEnv -> Module Ann -> BackendModule -> OptimizationSteps -> Aff Unit
     , onPrepareModule :: BuildEnv -> Module Ann -> Aff (Module Ann)
     }
  -> Aff Unit
basicBuildMain options = do
  coreFnModulesFromOutput options.coreFnDirectory options.coreFnGlobs >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit' 1
    Right coreFnModules -> do
      externalDirectives <- map (fromMaybe Map.empty) $ traverse externalDirectivesFromFile
        options.externalDirectivesFile
      coreFnModules # buildModules
        { directives: Map.union externalDirectives (parseDirectiveFile defaultDirectives).directives
        , foreignSemantics: coreForeignSemantics
        , onCodegenModule: options.onCodegenModule
        , onPrepareModule: options.onPrepareModule
        , traceIdents: Set.empty
        }

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

externalDirectivesFromFile :: FilePath -> Aff InlineDirectiveMap
externalDirectivesFromFile filePath = do
  fileContent <- FS.readTextFile UTF8 filePath
  let { errors, directives } = parseDirectiveFile fileContent
  for_ errors \(Tuple directive { position, error }) -> do
    Console.warn $ "Invalid directive [" <> show (position.line + 1) <> ":"
      <> show (position.column + 1)
      <> "]"
    Console.warn $ "  " <> directive
    Console.warn $ "  " <> printParseError error
  pure directives
