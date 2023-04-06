module PureScript.Backend.Chez.Syntax where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Regex as R
import Data.String.Regex.Flags as R.Flags
import Data.String.Regex.Unsafe as R.Unsafe
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Chez.Constants (libChezSchemePrefix, scmPrefixed)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), ModuleName(..), Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Syntax (Level(..))
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

definitionIdentifiers :: ChezDefinition -> Array String
definitionIdentifiers (DefineValue i _) = [ i ]
definitionIdentifiers (DefineCurriedFunction i _ _) = [ i ]
definitionIdentifiers (DefineUncurriedFunction i _ _) = [ i ]
definitionIdentifiers (DefineRecordType i [ x ]) =
  [ recordTypeCurriedConstructor i
  , recordTypePredicate i
  , recordTypeAccessor i x
  ]
definitionIdentifiers (DefineRecordType i fields) =
  [ recordTypeUncurriedConstructor i
  , recordTypePredicate i
  ] <> map (recordTypeAccessor i) fields

resolve :: ModuleName -> Qualified Ident -> String
resolve _ (Qualified Nothing (Ident i)) = i
resolve currentModule (Qualified (Just m) (Ident i))
  | currentModule == m = i
  | otherwise = coerce m <> "." <> i

toChezIdent :: Maybe Ident -> Level -> String
toChezIdent i (Level l) = case i of
  Just (Ident i') -> case i' of
    "$__unused" -> "_"
    _ -> i' <> show l
  Nothing -> "_" <> show l

jsonToChezString :: String -> String
jsonToChezString str = unicodeReplace str
  where
  unicodeRegex :: R.Regex
  unicodeRegex = R.Unsafe.unsafeRegex """\\u([A-F\d]{4})""" R.Flags.global

  unicodeReplace :: String -> String
  unicodeReplace s = R.replace' unicodeRegex unicodeReplaceMatch s

  unicodeReplaceMatch
    :: String
    -> Array (Maybe String)
    -> String
  unicodeReplaceMatch _ = case _ of
    [ (Just x) ] -> "\\x" <> x <> ";"
    _ -> unsafeCrashWith "Error matching at unicodeReplaceMatch in jsonToChezString"

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

library :: ModuleName -> Array Ident -> Array ChezExpr -> ChezExpr
library moduleName exports bindings =
  List $
    [ Identifier "library"
    , Identifier $ "(" <> coerce moduleName <> " lib" <> ")"
    , List $
        [ Identifier "export"
        ] <> (Identifier <$> coerce exports)
    , List
        [ Identifier "import"
        , List
            [ Identifier "prefix"
            , List
                [ Identifier "chezscheme"
                ]
            , Identifier libChezSchemePrefix
            ]
        ]
    ] <> bindings

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
