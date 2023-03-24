module PureScript.Backend.Chez.Runtime where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import PureScript.Backend.Chez.Syntax (ChezImport(..), ChezImportSet(..), ChezLibrary)

runtimeModule :: ChezLibrary
runtimeModule =
  { "#!chezscheme": true
  , "#!r6rs": true
  , name:
      { identifiers: NEA.cons' "_Chez_Runtime" [ "lib" ]
      , version: []
      }
  , exports: []
  , imports:
      [ ImportSet $ ImportLibrary { identifiers: NEA.singleton "chezscheme", version: Nothing }
      ]
  , body:
      { definitions: []
      , expressions: []
      }
  }
