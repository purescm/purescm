module PureScript.Backend.Chez.Runtime (runtimeModule) where

import Prelude

import Control.Monad.State (runState)
import Control.Monad.State.Class (class MonadState, modify_)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Chez.Constants (libChezSchemePrefix, scmPrefixed)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Chez.Syntax.ChezIdent (chezIdent)

makeExportedFunction
  :: forall m
   . Monad m
  => MonadState (Array ChezExport) m
  => String
  -> Array String
  -> ChezExpr
  -> m ChezDefinition
makeExportedFunction name args expr = do
  modify_ $ Array.cons $ ExportIdentifier $ chezIdent name
  pure $ DefineUncurriedFunction (chezIdent name) (map chezIdent args) expr

makeBooleanComparison
  :: forall m. Monad m => MonadState (Array ChezExport) m => String -> m ChezDefinition
makeBooleanComparison suffix =
  makeExportedFunction ("boolean" <> suffix) [ "x", "y" ] $
    S.List
      [ S.Identifier $ chezIdent $ scmPrefixed "fx" <> suffix
      , S.List [ S.Identifier $ chezIdent "boolean->integer", S.Identifier $ chezIdent "x" ]
      , S.List [ S.Identifier $ chezIdent "boolean->integer", S.Identifier $ chezIdent "y" ]
      ]

runtimeModule :: ChezLibrary
runtimeModule = do
  let
    Tuple definitions exports = flip runState [] $ sequence
      [ pure $ DefineUncurriedFunction (chezIdent "boolean->integer") [ chezIdent "x" ] $ S.List
          [ S.Identifier $ chezIdent $ scmPrefixed "if"
          , S.Identifier $ chezIdent "x"
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
      { identifiers: NEA.cons' (chezIdent "_Chez_Runtime") [ chezIdent "lib" ]
      , version: []
      }
  , exports
  , imports:
      [ ImportSet $ ImportPrefix
          (ImportLibrary { identifiers: NEA.singleton $ chezIdent "chezscheme", version: Nothing })
          (chezIdent libChezSchemePrefix)
      ]
  , body:
      { definitions
      , expressions: []
      }
  }
