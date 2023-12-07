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

makeExportedValue
  :: forall m. Monad m => MonadState (Array ChezExport) m => String -> ChezExpr -> m ChezDefinition
makeExportedValue name value = do
  modify_ $ Array.cons $ ExportIdentifier name
  pure $ S.Define name $ value

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
createMakeArrayFn = makeExportedValue "make-array" $ S.Identifier "srfi:214:flexvector"

createArrayRefFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
createArrayRefFn = makeExportedValue "array-ref" $ S.Identifier "srfi:214:flexvector-ref"

createArrayLengthFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
createArrayLengthFn = makeExportedValue "array-length" $ S.Identifier "srfi:214:flexvector-length"

createMakeObjectFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
createMakeObjectFn = makeExportedValue "make-object" $ S.List
  [ S.Identifier $ scmPrefixed "lambda"
  , S.Identifier "args"
  , S.List
      [ S.Identifier "srfi:125:alist->hash-table"
      , S.Identifier "args"
      , S.Identifier "string-comparator"
      ]
  ]

createObjectRefFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
createObjectRefFn = makeExportedValue "object-ref" $ S.Identifier "srfi:125:hash-table-ref"

createObjectSetFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
createObjectSetFn = makeExportedValue "object-set!" $ S.Identifier "srfi:125:hash-table-set!"

createObjectCopyFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
createObjectCopyFn = makeExportedFunction "object-copy" (NEA.singleton "v") $ S.List
  [ S.Identifier "srfi:125:hash-table-copy"
  , S.Identifier "v"
  , S.Bool true
  ]

-- Curried version of `cons`
listConsFn :: forall m. Monad m => MonadState (Array ChezExport) m => m ChezDefinition
listConsFn = makeExportedValue "cons" $
  S.List
    [ S.Identifier (scmPrefixed "lambda")
    , S.List [ S.Identifier "x" ]
    , S.List
        [ S.Identifier $ scmPrefixed "lambda"
        , S.List [ S.Identifier "xs" ]
        , S.List [ S.Identifier $ scmPrefixed "cons", S.Identifier "x", S.Identifier "xs" ]
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
          , S.Identifier "bytestring?"
          , S.Identifier "bytestring=?"
          , S.Identifier "bytestring<?"
          , S.Identifier "bytestring-hash"
          ]
      , makeBooleanComparison ">?"
      , makeBooleanComparison ">=?"
      , makeBooleanComparison "<=?"
      , makeBooleanComparison "<?"
      , createMakeArrayFn
      , createArrayRefFn
      , createArrayLengthFn
      , createMakeObjectFn
      , createObjectRefFn
      , createObjectSetFn
      , createObjectCopyFn
      , listConsFn
      ]
  { "#!chezscheme": true
  , "#!r6rs": true
  , name:
      { identifiers: NEA.cons' "purs" [ "runtime", moduleLib ]
      , version: []
      }
  , exports: exports <>
      [ ExportIdentifier "string->bytestring"
      , ExportIdentifier "bytestring-append"
      , ExportIdentifier "bytestring=?"
      , ExportIdentifier "bytestring>?"
      , ExportIdentifier "bytestring>=?"
      , ExportIdentifier "bytestring<?"
      , ExportIdentifier "bytestring<=?"
      ]
  , imports:
      [ ImportSet $ ImportPrefix
          (ImportLibrary { identifiers: NEA.singleton "chezscheme", version: Nothing })
          libChezSchemePrefix
      , ImportSet $ ImportOnly
          ( ImportLibrary
              { identifiers: NEA.cons' "purs" [ "runtime", "bytestring" ], version: Nothing }
          )
          [ "string->bytestring"
          , "bytestring?"
          , "bytestring=?"
          , "bytestring<?"
          , "bytestring>?"
          , "bytestring<=?"
          , "bytestring>=?"
          , "bytestring-hash"
          , "bytestring-append"
          ]
      , importSRFI "125"
      , importSRFI "128"
      , importSRFI "214"
      ]
  , body:
      { definitions
      , expressions: []
      }
  }
