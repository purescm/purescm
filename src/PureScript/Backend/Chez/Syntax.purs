module PureScript.Backend.Chez.Syntax where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import PureScript.Backend.Chez.Constants (scmPrefixed)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Prop(..))
import Safe.Coerce (coerce)

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
  = DefineValue String ChezExpr
  | DefineCurriedFunction String (NonEmptyArray String) ChezExpr
  | DefineUncurriedFunction String (Array String) ChezExpr
  | DefineRecordType String (Array String)

newtype LiteralDigit = LiteralDigit String

derive instance Newtype LiteralDigit _
derive newtype instance Eq LiteralDigit
derive newtype instance Ord LiteralDigit

data ChezExpr
  = Integer LiteralDigit
  | Float LiteralDigit
  | Char String
  | StringExpr String
  | Bool Boolean
  | Identifier String
  | List (Array ChezExpr)

--

chezCond :: NonEmptyArray { c :: ChezExpr, e :: ChezExpr } -> Maybe ChezExpr -> ChezExpr
chezCond b o =
  let
    b' :: Array ChezExpr
    b' = NonEmptyArray.toArray b <#> \{ c, e } -> List [ c, e ]

    o' :: Array ChezExpr
    o' = Array.fromFoldable o <#> \x -> List [ Identifier $ scmPrefixed "else", x ]
  in
    List $ [ Identifier $ scmPrefixed "cond" ] <> b' <> o'

chezUncurriedApplication :: ChezExpr -> Array ChezExpr -> ChezExpr
chezUncurriedApplication f s = List $ Array.cons f s

chezUncurriedFunction :: Array String -> ChezExpr -> ChezExpr
chezUncurriedFunction a e = List
  [ Identifier $ scmPrefixed "lambda", List $ Identifier <$> a, e ]

chezCurriedApplication :: ChezExpr -> NonEmptyArray ChezExpr -> ChezExpr
chezCurriedApplication f s = NonEmptyArray.foldl1 app $ NonEmptyArray.cons f s

chezCurriedFunction :: NonEmptyArray String -> ChezExpr -> ChezExpr
chezCurriedFunction a e = Array.foldr lambda e $ NonEmptyArray.toArray a

chezThunk :: ChezExpr -> ChezExpr
chezThunk e = List [ Identifier $ scmPrefixed "lambda", List [], e ]

chezUnthunk :: ChezExpr -> ChezExpr
chezUnthunk e = List [ e ]

--

app :: ChezExpr -> ChezExpr -> ChezExpr
app f x = List [ f, x ]

define :: Ident -> ChezExpr -> ChezExpr
define i e = List [ Identifier $ scmPrefixed "define", Identifier $ coerce i, e ]

record :: Array (Prop ChezExpr) -> ChezExpr
record r =
  let
    field :: Prop ChezExpr -> ChezExpr
    field (Prop k v) =
      List
        [ Identifier $ scmPrefixed "hashtable-set!"
        , Identifier "$record"
        , StringExpr $ Json.stringify $ Json.fromString k
        , v
        ]
  in
    List $
      [ Identifier $ scmPrefixed "letrec*"
      , List
          [ List
              [ Identifier "$record"
              , List
                  [ Identifier $ scmPrefixed "make-hashtable"
                  , Identifier $ scmPrefixed "string-hash"
                  , Identifier $ scmPrefixed "string=?"
                  ]
              ]
          ]
      ] <> (field <$> r) <> [ Identifier "$record" ]

quote :: ChezExpr -> ChezExpr
quote e = app (Identifier $ scmPrefixed "quote") e

eqQ :: ChezExpr -> ChezExpr -> ChezExpr
eqQ x y = chezUncurriedApplication (Identifier $ scmPrefixed "eq?") [ x, y ]

lambda :: String -> ChezExpr -> ChezExpr
lambda a e = List [ Identifier $ scmPrefixed "lambda", List [ Identifier a ], e ]

vector :: Array ChezExpr -> ChezExpr
vector = List <<< Array.cons (Identifier $ scmPrefixed "vector")

recordTypeName :: String -> String
recordTypeName i = i <> "$"

recordTypeCurriedConstructor :: String -> String
recordTypeCurriedConstructor i = i

recordTypeUncurriedConstructor :: String -> String
recordTypeUncurriedConstructor i = i <> "*"

recordTypePredicate :: String -> String
recordTypePredicate i = i <> "?"

recordTypeAccessor :: String -> String -> String
recordTypeAccessor i field = i <> "-" <> field

recordAccessor :: ChezExpr -> String -> String -> ChezExpr
recordAccessor expr name field =
  chezUncurriedApplication (Identifier $ recordTypeAccessor name field) [ expr ]
