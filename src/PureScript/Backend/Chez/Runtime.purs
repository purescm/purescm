module PureScript.Backend.Chez.Runtime (runtimeModule) where

import Prelude

import Control.Monad.State (runState)
import Control.Monad.State.Class (class MonadState, modify_)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Chez.Constants (libChezSchemePrefix, scmPrefixed)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S

makeExportedValue
  :: forall m
   . Monad m
  => MonadState (Array ChezExport) m
  => String
  -> ChezExpr
  -> m ChezDefinition
makeExportedValue name expr = do
  modify_ $ Array.cons $ ExportIdentifier name
  pure $ DefineValue name expr

makeExportedFunction
  :: forall m
   . Monad m
  => MonadState (Array ChezExport) m
  => String
  -> Array String
  -> ChezExpr
  -> m ChezDefinition
makeExportedFunction name args expr = do
  modify_ $ Array.cons $ ExportIdentifier name
  pure $ DefineUncurriedFunction name args expr

makeBooleanComparison
  :: forall m
   . Monad m
  => MonadState (Array ChezExport) m
  => String
  -> m ChezDefinition
makeBooleanComparison suffix =
  makeExportedFunction ("boolean" <> suffix) [ "x", "y" ] $
    S.List
      [ S.Identifier $ scmPrefixed "fx" <> suffix
      , S.List [ S.Identifier "boolean->integer", S.Identifier "x" ]
      , S.List [ S.Identifier "boolean->integer", S.Identifier "y" ]
      ]

runtimeModule :: ChezLibrary
runtimeModule = do
  let
    Tuple definitions exports = flip runState [] $ sequence
      [ pure $ DefineUncurriedFunction "boolean->integer" [ "x" ] $ S.List
          [ S.Identifier $ scmPrefixed "if"
          , S.Identifier "x"
          , S.Integer $ S.LiteralDigit "1"
          , S.Integer $ S.LiteralDigit "0"
          ]
      , makeBooleanComparison ">?"
      , makeBooleanComparison ">=?"
      , makeBooleanComparison "<=?"
      , makeBooleanComparison "<?"
      , defaultHashtableValue
      , hashtableGet
      ]
  { "#!chezscheme": true
  , "#!r6rs": true
  , name:
      { identifiers: NEA.cons' "_Chez_Runtime" [ "lib" ]
      , version: []
      }
  , exports
  , imports:
      [ ImportSet $ ImportPrefix
          (ImportLibrary { identifiers: NEA.singleton "chezscheme", version: Nothing })
          libChezSchemePrefix
      ]
  , body:
      { definitions
      , expressions: []
      }
  }

defaultHashtableValue
  :: forall m
   . Monad m
  => MonadState (Array ChezExport) m
  => m ChezDefinition
defaultHashtableValue = makeExportedValue "default-ht-value" $
  S.chezUncurriedApplication
    (S.Identifier $ scmPrefixed "gensym")
    [ S.String "default-ht-value" ]

hashtableGet
  :: forall m
   . Monad m
  => MonadState (Array ChezExport) m
  => m ChezDefinition
hashtableGet = makeExportedFunction "hashtable-get" [ "ht", "key" ]
  $ S.let_
      [ Tuple
          "value" $
          S.hashtableRef
            (S.Identifier "ht")
            (S.Identifier "key")
            (S.Identifier "default-ht-value")
      ]
  $ S.chezCond
      ( NEA.singleton
          { c: S.eqQ
              (S.Identifier "value")
              (S.Identifier "default-ht-value")
          , e: S.chezUncurriedApplication
              (S.Identifier $ scmPrefixed "error")
              [ S.Boolean false
              , S.String $ Json.stringify $ Json.fromString "oops!"
              , S.Boolean false
              ]
          }
      )
      (Just (S.Identifier "value"))
