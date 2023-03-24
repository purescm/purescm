module PureScript.Backend.Chez.Runtime where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S

runtimeModule :: ChezLibrary
runtimeModule =
  { "#!chezscheme": true
  , "#!r6rs": true
  , name:
      { identifiers: NEA.cons' "_Chez_Runtime" [ "lib" ]
      , version: []
      }
  , exports:
      [ ExportIdentifier "boolean>?"
      , ExportIdentifier "boolean>=?"
      , ExportIdentifier "boolean<=?"
      , ExportIdentifier "boolean<?"
      ]
  , imports:
      [ ImportSet $ ImportPrefix
          (ImportLibrary { identifiers: NEA.singleton "chezscheme", version: Nothing })
          "scm:"
      ]
  , body:
      { definitions:
          [ DefineUncurriedFunction "boolean->integer" [ "x" ] $ S.List
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
      , expressions: []
      }
  }

makeBooleanComparison :: String -> ChezDefinition
makeBooleanComparison suffix =
  DefineUncurriedFunction ("boolean" <> suffix) [ "x", "y" ] $
    S.List
      [ S.Identifier $ "scm:fx" <> suffix
      , S.List [ S.Identifier "boolean->integer", S.Identifier "x" ]
      , S.List [ S.Identifier "boolean->integer", S.Identifier "y" ]
      ]
