module PureScript.Backend.Chez.Syntax where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Chez.Constants (scmPrefixed)
import PureScript.Backend.Optimizer.CoreFn (Prop(..))

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

data ChezExpr
  = Integer LiteralDigit
  | Float LiteralDigit
  | Char String
  | StringExpr String
  | Bool Boolean
  | Identifier String
  | List (Array ChezExpr)
  | Cond (NonEmptyArray (Tuple ChezExpr ChezExpr)) (Maybe ChezExpr)
  | Let Boolean (NonEmptyArray (Tuple String ChezExpr)) ChezExpr
  | Lambda (Array String) ChezExpr

--

runUncurriedFn :: ChezExpr -> Array ChezExpr -> ChezExpr
runUncurriedFn f s = List $ Array.cons f s

mkUncurriedFn :: Array String -> ChezExpr -> ChezExpr
mkUncurriedFn a e = Lambda a e

runCurriedFn :: ChezExpr -> NonEmptyArray ChezExpr -> ChezExpr
runCurriedFn f s = NonEmptyArray.foldl1 app $ NonEmptyArray.cons f s

mkCurriedFn :: NonEmptyArray String -> ChezExpr -> ChezExpr
mkCurriedFn a e = Array.foldr (Lambda <<< Array.singleton) e $ NonEmptyArray.toArray a

thunk :: ChezExpr -> ChezExpr
thunk e = Lambda [] e

unthunk :: ChezExpr -> ChezExpr
unthunk e = List [ e ]

--

app :: ChezExpr -> ChezExpr -> ChezExpr
app f x = List [ f, x ]

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
eqQ x y = runUncurriedFn (Identifier $ scmPrefixed "eq?") [ x, y ]

vector :: Array ChezExpr -> ChezExpr
vector = List <<< Array.cons (Identifier $ scmPrefixed "vector")

hashtableCopy :: ChezExpr -> ChezExpr
hashtableCopy h = runUncurriedFn (Identifier $ scmPrefixed "hashtable-copy") [ h ]

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
  runUncurriedFn (Identifier $ recordTypeAccessor name field) [ expr ]

recordUpdate :: ChezExpr -> Array (Prop ChezExpr) -> ChezExpr
recordUpdate h f = do
  let
    field :: Prop ChezExpr -> ChezExpr
    field (Prop k v) =
      List
        [ Identifier $ scmPrefixed "hashtable-set!"
        , Identifier "$record"
        , StringExpr $ Json.stringify $ Json.fromString k
        , v
        ]
  Let false (NonEmptyArray.singleton (Tuple "$record" (hashtableCopy h))) (List (field <$> f))
