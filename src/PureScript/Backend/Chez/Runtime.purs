module PureScript.Backend.Chez.Runtime (runtimeModule) where

import Prelude

import Control.Monad.State (runState)
import Control.Monad.State.Class (class MonadState, modify_)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Chez.Constants (libChezSchemePrefix, moduleLib, scmPrefixed)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S

makeExportedFunction
  :: forall m
   . Monad m
  => MonadState (Array ChezExport) m
  => String
  -> NonEmptyArray String
  -> ChezExpr
  -> m ChezDefinition
makeExportedFunction name args expr = do
  modify_ $ Array.cons $ ExportIdentifier name
  pure $ Define name $ S.mkCurriedFn args expr

makeBooleanComparison
  :: forall m. Monad m => MonadState (Array ChezExport) m => String -> m ChezDefinition
makeBooleanComparison suffix =
  makeExportedFunction ("boolean" <> suffix) (NonEmptyArray [ "x", "y" ]) $
    S.List
      [ S.Identifier $ scmPrefixed "fx" <> suffix
      , S.List [ S.Identifier "boolean->integer", S.Identifier "x" ]
      , S.List [ S.Identifier "boolean->integer", S.Identifier "y" ]
      ]

createMakeArrayFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
createMakeArrayFn = do
  modify_ $ Array.cons $ ExportIdentifier "make-array"
  pure $ S.Define "make-array" $ S.Identifier "srfi:214:flexvector"

createMakeObjectFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
createMakeObjectFn = do
  modify_ $ Array.cons $ ExportIdentifier "make-object"
  pure $ S.Define "make-object" $ S.List
    [ S.Identifier $ scmPrefixed "lambda"
    , S.Identifier "args"
    , S.List
        [ S.Identifier $ scmPrefixed "apply"
        , S.Identifier "srfi:125:hash-table"
        , S.List
            [ S.Identifier $ scmPrefixed "cons"
            , S.Identifier "string-comparator"
            , S.Identifier "args"
            ]
        ]
    ]

importSRFI :: String -> ChezImport
importSRFI srfi = ImportSet $ ImportPrefix
  ( ImportLibrary
      { identifiers: NEA.cons' "purs" [ "runtime", "srfi", ":" <> srfi ], version: Nothing }
  )
  ("srfi:" <> srfi <> ":")

runtimeModule :: ChezLibrary
runtimeModule = do
  let
    Tuple definitions exports = flip runState [] $ sequence
      [ pure $ S.Define "boolean->integer" $ S.mkCurriedFn (pure "x") $ S.List
          [ S.Identifier $ scmPrefixed "if"
          , S.Identifier "x"
          , S.Integer $ S.LiteralDigit "1"
          , S.Integer $ S.LiteralDigit "0"
          ]
      , pure $ S.Define "string-comparator" $ S.List
          [ S.Identifier "srfi:128:make-comparator"
          , S.Identifier $ scmPrefixed "string?"
          , S.Identifier $ scmPrefixed "string=?"
          , S.Identifier $ scmPrefixed "string<?"
          , S.Identifier $ scmPrefixed "string-hash"
          ]
      , makeBooleanComparison ">?"
      , makeBooleanComparison ">=?"
      , makeBooleanComparison "<=?"
      , makeBooleanComparison "<?"
      , createMakeArrayFn
      , createMakeObjectFn
      ]
  { "#!chezscheme": true
  , "#!r6rs": true
  , name:
      { identifiers: NEA.cons' "purs" [ "runtime", moduleLib ]
      , version: []
      }
  , exports
  , imports:
      [ ImportSet $ ImportPrefix
          (ImportLibrary { identifiers: NEA.singleton "chezscheme", version: Nothing })
          libChezSchemePrefix
      , importSRFI "125"
      , importSRFI "128"
      , importSRFI "214"
      ]
  , body:
      { definitions
      , expressions: []
      }
  }
