module PureScript.Backend.Chez.Runtime where

import Prelude

import Control.Monad.State (runState)
import Control.Monad.State.Class (class MonadState, modify_)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S

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
  :: forall m. Monad m => MonadState (Array ChezExport) m => String -> m ChezDefinition
makeBooleanComparison suffix =
  makeExportedFunction ("boolean" <> suffix) [ "x", "y" ] $
    S.List
      [ S.Identifier $ "scm:fx" <> suffix
      , S.List [ S.Identifier "boolean->integer", S.Identifier "x" ]
      , S.List [ S.Identifier "boolean->integer", S.Identifier "y" ]
      ]

runtimeModule :: ChezLibrary
runtimeModule = do
  let
    Tuple definitions exports = flip runState [] $ sequence
      [ pure $ DefineUncurriedFunction "boolean->integer" [ "x" ] $ S.List
          [ S.Identifier "scm:if"
          , S.Identifier "x"
          , S.Integer $ S.LiteralDigit "1"
          , S.Integer $ S.LiteralDigit "0"
          ]
      , makeBooleanComparison ">?"
      , makeBooleanComparison ">=?"
      , makeBooleanComparison "<=?"
      , makeBooleanComparison "<?"
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
          "scm:"
      ]
  , body:
      { definitions
      , expressions: []
      }
  }
