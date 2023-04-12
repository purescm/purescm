module PureScript.Backend.Chez.Syntax where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)

type ChezLibrary =
  { "#!r6rs" :: Boolean
  , "#!chezscheme" :: Boolean
  , name :: LibraryName
  , exports :: Array ChezExport
  , imports :: Array ChezImport
  , body :: LibraryBody
  }

type LibraryName =
  { identifiers :: NonEmptyArray String
  , version :: Array LibraryVersion
  }

newtype LibraryVersion = LibraryVersion Prim.Int

derive instance Newtype LibraryVersion _

data ChezExport
  = ExportIdentifier String
  | ExportRename (Array { original :: String, rename :: String })

type LibraryReference =
  { identifiers :: NonEmptyArray String
  , version :: Maybe VersionReference
  }

data VersionReference
  = VersionRef (NonEmptyArray SubVersionReference)
  | VersionAnd (Array VersionReference)
  | VersionOr (Array VersionReference)
  | VersionNot VersionReference

data SubVersionReference
  = SubVersionRef LibraryVersion
  | SubVersionGTE LibraryVersion
  | SubVersionLTE LibraryVersion
  | SubVersionAnd (Array SubVersionReference)
  | SubVersionOr (Array SubVersionReference)
  | SubVersionNot SubVersionReference

data ChezImport
  = ImportSet ChezImportSet
  | ImportFor ChezImportSet (Array ChezImportLevel)

data ChezImportLevel
  = ImportLevelRun
  | ImportLevelExpand
  | ImportLevelMeta Prim.Int

data ChezImportSet
  = ImportLibrary LibraryReference
  | ImportOnly ChezImportSet (Array String)
  | ImportExcept ChezImportSet (Array String)
  | ImportPrefix ChezImportSet String
  | ImportRename ChezImportSet (Array { original :: String, rename :: String })

type LibraryBody =
  { definitions :: Array ChezDefinition
  , expressions :: Array ChezExpr
  }

data ChezDefinition
  = Define String ChezExpr
  | DefineRecordType String (Array String)

newtype LiteralDigit = LiteralDigit String

derive instance Newtype LiteralDigit _
derive newtype instance Eq LiteralDigit
derive newtype instance Ord LiteralDigit

newtype ChezString = ChezString String

derive instance Newtype ChezString _
derive newtype instance Eq ChezString
derive newtype instance Ord ChezString

data ChezExpr
  = Integer LiteralDigit
  | Float LiteralDigit
  | Char String
  | StringExpr ChezString
  | Bool Boolean
  | Identifier String
  | List (Array ChezExpr)
  | Cond (NonEmptyArray (Tuple ChezExpr ChezExpr)) (Maybe ChezExpr)
  | Let Boolean (NonEmptyArray (Tuple String ChezExpr)) ChezExpr
  | Lambda (Array String) ChezExpr
