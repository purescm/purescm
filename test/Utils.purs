-- A majority of the code below was copied from 
-- https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/test/Utils.purs
-- https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/src/Main.purs
--
-- To fullfill copyright requirements...
--    Copyright © 2022 Arista Networks, Inc.
--    MIT license: https://opensource.org/license/mit/
module Test.Utils where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, effectCanceler, error, makeAff, message, throwError)
import Effect.Class (liftEffect)
import Node.EventEmitter (on_)
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Library.Execa (ExecaResult, execa)
import Node.Path (FilePath)
import Node.Process as Process
import Node.Stream as Stream
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types as CSTT

spawnFromParent :: String -> Array String -> Aff Unit
spawnFromParent command args = do
  spawned <- execa command args identity
  spawned.stdout.pipeToParentStdout
  spawned.stderr.pipeToParentStderr
  result <- spawned.getResult
  unless (result.exitCode == Just 0) do
    liftEffect $ Process.exit' 1

execWithStdin :: String -> Array String -> String -> Aff ExecaResult
execWithStdin command args input = do
  spawned <- execa command args identity
  spawned.stdin.writeUtf8End input
  spawned.getResult

mkdirp :: FilePath -> Aff Unit
mkdirp path = FS.mkdir' path { recursive: true, mode: Perms.permsAll }

loadModuleMain
  :: { libdir :: FilePath
     , scheme :: String
     , hasMain :: Boolean
     , moduleName :: String
     , modulePath :: String
     }
  -> Aff ExecaResult
loadModuleMain options = do
  let
    arguments :: Array String
    arguments = [ "-q", "--libdirs", options.libdir <> ":" ] <>
      if not options.hasMain then
        [ "--script", options.modulePath ]
      else
        []
  spawned <- execa options.scheme arguments identity
  when options.hasMain do
    spawned.stdin.writeUtf8End $ Array.fold
      [ "(base-exception-handler (lambda (e) (display-condition e (console-error-port)) (newline (console-error-port)) (exit -1)))"
      , "(top-level-program (import (" <> options.moduleName <> " lib)) (main))"
      ]
  spawned.stdout.pipeToParentStdout
  spawned.stderr.pipeToParentStderr
  spawned.getResult

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  makeAff \k -> do
    src <- createReadStream from
    dst <- createWriteStream to
    src # on_ Stream.errorH \err -> do
      Stream.destroy' dst $ error $ "Got error in src stream: " <> message err
      k $ Left err
    dst # on_ Stream.errorH (k <<< Left)
    Stream.pipe src dst
    pure $ effectCanceler do
      Stream.destroy dst
      Stream.destroy src

-- | Returns `Right true` if the source code has this type signature
-- | somewhere in it:
-- | ```
-- | main :: Effect Unit
-- | ```
-- |
-- | If `Effect` or `Unit` are qualified by a module alias,
-- | this will not return `true`.
-- | ```
-- | main :: Effect.Effect Prelude.Unit
-- | ```
canRunMain :: String -> Either String Boolean
canRunMain sourceCode =
  case parseModule sourceCode of
    ParseSucceeded (CSTT.Module { body: CSTT.ModuleBody { decls } }) ->
      pure $ Array.any isMain decls
    ParseSucceededWithErrors _ errs ->
      Left $ Array.intercalate "\n"
        [ "canRunMain - could not completely parse file."
        , Array.intercalate "\n" $ map printPositionedError $ NonEmptyArray.toArray errs
        ]
    ParseFailed err ->
      Left $ Array.intercalate "\n"
        [ "canRunMain - could not parse file."
        , printPositionedError err
        ]
  where
  printPositionedError err = Array.intercalate "\n"
    [ ""
    , "Position: " <> show err.position
    , "Reason: " <> printParseError err.error
    ]
  isMain = case _ of
    CSTT.DeclSignature
      ( CSTT.Labeled
          { label: CSTT.Name { name: CSTT.Ident "main" }
          , value:
              CSTT.TypeApp
                (CSTT.TypeConstructor (CSTT.QualifiedName { name: CSTT.Proper "Effect" }))
                ( NonEmptyArray
                    [ CSTT.TypeConstructor (CSTT.QualifiedName { name: CSTT.Proper "Unit" })
                    ]
                )
          }
      ) -> true
    _ -> false
