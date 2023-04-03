module PureScript.Backend.Chez.Syntax where

import Prelude

import Control.Alternative as Alternative
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un)
import Data.String.Regex as R
import Data.String.Regex.Flags as R.Flags
import Data.String.Regex.Unsafe as R.Unsafe
import Dodo (Doc)
import Dodo as D
import Partial.Unsafe (unsafeCrashWith)
import Prim as Prim
import PureScript.Backend.Chez.Constants (libChezSchemePrefix, scmPrefixed)
import PureScript.Backend.Chez.Syntax.ChezIdent (EscapedIdent, chezIdent, unEscape)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), ModuleName(..), Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Syntax (Level(..))
import Safe.Coerce (coerce)

type ChezLibrary =
  { "#!r6rs" :: Prim.Boolean
  , "#!chezscheme" :: Prim.Boolean
  , name :: LibraryName
  , exports :: Prim.Array ChezExport
  , imports :: Prim.Array ChezImport
  , body :: LibraryBody
  }

type LibraryName =
  { identifiers :: NonEmptyArray EscapedIdent
  , version :: Prim.Array LibraryVersion
  }

newtype LibraryVersion = LibraryVersion Prim.Int

derive instance Newtype LibraryVersion _

data ChezExport
  = ExportIdentifier EscapedIdent
  | ExportRename (Prim.Array { original :: EscapedIdent, rename :: EscapedIdent })

type LibraryReference =
  { identifiers :: NonEmptyArray EscapedIdent
  , version :: Maybe VersionReference
  }

data VersionReference
  = VersionRef (NonEmptyArray SubVersionReference)
  | VersionAnd (Prim.Array VersionReference)
  | VersionOr (Prim.Array VersionReference)
  | VersionNot VersionReference

data SubVersionReference
  = SubVersionRef LibraryVersion
  | SubVersionGTE LibraryVersion
  | SubVersionLTE LibraryVersion
  | SubVersionAnd (Prim.Array SubVersionReference)
  | SubVersionOr (Prim.Array SubVersionReference)
  | SubVersionNot SubVersionReference

data ChezImport
  = ImportSet ChezImportSet
  | ImportFor ChezImportSet (Prim.Array ChezImportLevel)

data ChezImportLevel
  = ImportLevelRun
  | ImportLevelExpand
  | ImportLevelMeta Prim.Int

data ChezImportSet
  = ImportLibrary LibraryReference
  | ImportOnly ChezImportSet (Prim.Array EscapedIdent)
  | ImportExcept ChezImportSet (Prim.Array EscapedIdent)
  | ImportPrefix ChezImportSet EscapedIdent
  | ImportRename ChezImportSet (Prim.Array { original :: EscapedIdent, rename :: EscapedIdent })

type LibraryBody =
  { definitions :: Prim.Array ChezDefinition
  , expressions :: Prim.Array ChezExpr
  }

data ChezDefinition
  = DefineValue EscapedIdent ChezExpr
  | DefineCurriedFunction EscapedIdent (NonEmptyArray EscapedIdent) ChezExpr
  | DefineUncurriedFunction EscapedIdent (Prim.Array EscapedIdent) ChezExpr
  | DefineRecordType EscapedIdent (Prim.Array EscapedIdent)

newtype LiteralDigit = LiteralDigit Prim.String

derive instance Newtype LiteralDigit _
derive newtype instance Eq LiteralDigit
derive newtype instance Ord LiteralDigit

data ChezExpr
  = Integer LiteralDigit
  | Float LiteralDigit
  | String Prim.String
  | Boolean Prim.Boolean
  | Identifier EscapedIdent
  | List (Prim.Array ChezExpr)

definitionIdentifiers :: ChezDefinition -> Prim.Array EscapedIdent
definitionIdentifiers (DefineValue i _) = [ i ]
definitionIdentifiers (DefineCurriedFunction i _ _) = [ i ]
definitionIdentifiers (DefineUncurriedFunction i _ _) = [ i ]
definitionIdentifiers (DefineRecordType i [ x ]) =
  [ chezIdent $ recordTypeCurriedConstructor $ unEscape i
  , chezIdent $ recordTypePredicate $ unEscape i
  , chezIdent $ recordTypeAccessor (unEscape i) $ unEscape x
  ]
definitionIdentifiers (DefineRecordType i fields) =
  [ chezIdent $ recordTypeUncurriedConstructor $ unEscape i
  , chezIdent $ recordTypePredicate $ unEscape i
  ] <> map (\field -> chezIdent $ recordTypeAccessor (unEscape i) $ unEscape field) fields

resolve :: ModuleName -> Qualified Ident -> Prim.String
resolve _ (Qualified Nothing (Ident i)) = i
resolve currentModule (Qualified (Just m) (Ident i))
  | currentModule == m = i
  | otherwise = coerce m <> "." <> i

printWrap :: Doc Void -> Doc Void -> Doc Void -> Doc Void
printWrap l r x = l <> x <> r

printList :: Doc Void -> Doc Void
printList = printWrap (D.text "(") (D.text ")")

printNamedList :: Prim.String -> Doc Void -> Doc Void
printNamedList name body
  | D.isEmpty body =
      printList $ D.text name
  | otherwise =
      printList $ D.words [ D.text name, body ]

printIndentedList :: Doc Void -> Doc Void -> Doc Void
printIndentedList ident body
  | D.isEmpty body =
      printList ident
  | otherwise =
      D.lines
        [ D.text "(" <> ident
        , D.indent body <> D.text ")"
        ]

linesSeparated :: Prim.Array (Doc Void) -> Doc Void
linesSeparated = Array.intercalate twoLineBreaks
  where
  twoLineBreaks = D.break <> D.break

printLibrary :: ChezLibrary -> Doc Void
printLibrary lib =
  flip append D.break
    $ D.lines
    $ Array.catMaybes
        [ D.text "#!r6rs" <$ Alternative.guard lib."#!r6rs"
        , D.text "#!chezscheme" <$ Alternative.guard lib."#!chezscheme"
        , Just $ printNamedIndentedList (D.text "library") $ D.lines
            [ printLibraryName lib.name
            , printIndentedList (D.text "export") $ D.lines $ map printExport lib.exports
            , (printIndentedList (D.text "import") $ D.lines $ map printImport lib.imports)
                <> D.break
            , printBody lib.body
            ]
        ]

printLibraryName :: LibraryName -> Doc Void
printLibraryName { identifiers, version } =
  printList $ D.words
    [ D.words $ map (D.text <<< unEscape) identifiers
    , case version of
        [] -> mempty
        _ -> printList $ D.words $ map printLibraryVersion version
    ]

printLibraryVersion :: LibraryVersion -> Doc Void
printLibraryVersion = D.text <<< show <<< un LibraryVersion

printExport :: ChezExport -> Doc Void
printExport = case _ of
  ExportIdentifier s -> D.text $ unEscape s
  ExportRename arr ->
    printNamedList "rename"
      $ D.words
      $ map (\r -> printList $ D.words [ D.text $ unEscape r.original, D.text $ unEscape r.rename ])
          arr

printImport :: ChezImport -> Doc Void
printImport = case _ of
  ImportSet is -> printImportSet is
  ImportFor is lvls ->
    printNamedList "for" $ D.words
      [ printImportSet is
      , D.words $ map printImportLevel lvls
      ]

printImportLevel :: ChezImportLevel -> Doc Void
printImportLevel = case _ of
  ImportLevelRun -> D.text "run"
  ImportLevelExpand -> D.text "expand"
  ImportLevelMeta level -> printNamedList "meta" $ D.text $ show level

printImportSet :: ChezImportSet -> Doc Void
printImportSet = case _ of
  ImportLibrary libRef ->
    printLibraryReference libRef
  ImportOnly impSet identifiers ->
    printNamedList "only" $ D.words
      [ printImportSet impSet, D.words $ map (D.text <<< unEscape) identifiers ]
  ImportExcept impSet identifiers ->
    printNamedList "except" $ D.words
      [ printImportSet impSet, D.words $ map (D.text <<< unEscape) identifiers ]
  ImportPrefix impSet rename ->
    printNamedList "prefix" $ D.words [ printImportSet impSet, (D.text <<< unEscape) rename ]
  ImportRename impSet identifiers ->
    printNamedList "rename" $ D.words
      [ printImportSet impSet
      , D.words $ map
          (\r -> printList $ D.words $ map (D.text <<< unEscape) [ r.original, r.rename ])
          identifiers
      ]

printLibraryReference :: LibraryReference -> Doc Void
printLibraryReference lib = do
  let
    printListFn
      | Array.elem (unEscape $ NEA.head lib.identifiers)
          [ "for", "library", "only", "except", "prefix", "rename" ] =
          printNamedList "library"
      | otherwise =
          printList
  printListFn $ D.words
    [ D.words $ map (D.text <<< unEscape) lib.identifiers
    , maybe mempty printVersionReference lib.version
    ]

printVersionReference :: VersionReference -> Doc Void
printVersionReference = case _ of
  VersionRef ref ->
    printList $ D.words $ map printSubVersionReference ref
  VersionAnd ref ->
    printNamedList "and" $ D.words $ map printVersionReference ref
  VersionOr ref ->
    printNamedList "or" $ D.words $ map printVersionReference ref
  VersionNot ref ->
    printNamedList "not" $ printVersionReference ref

printSubVersionReference :: SubVersionReference -> Doc Void
printSubVersionReference = case _ of
  SubVersionRef ref -> printList $ printLibraryVersion ref
  SubVersionGTE ref -> printNamedList ">=" $ printLibraryVersion ref
  SubVersionLTE ref -> printNamedList "<=" $ printLibraryVersion ref
  SubVersionAnd ref -> printNamedList "and" $ D.words
    $ map printSubVersionReference ref
  SubVersionOr ref -> printNamedList "or" $ D.words
    $ map printSubVersionReference ref
  SubVersionNot ref -> printNamedList "not" $ printSubVersionReference ref

printBody :: LibraryBody -> Doc Void
printBody lib = do
  let
    defs = map printDefinition lib.definitions
    exprs = map printChezExpr lib.expressions
  linesSeparated $ defs <> exprs

printNamedIndentedList :: Doc Void -> Doc Void -> Doc Void
printNamedIndentedList firstLine body
  | D.isEmpty body =
      printList firstLine
  | otherwise =
      D.lines
        [ D.text "(" <> firstLine
        , D.indent $ body <> D.text ")"
        ]

printDefinition :: ChezDefinition -> Doc Void
printDefinition = case _ of
  DefineValue ident expr ->
    printNamedIndentedList (D.words [ D.text $ scmPrefixed "define", D.text $ unEscape ident ])
      $ printChezExpr expr
  DefineCurriedFunction ident args expr ->
    printNamedIndentedList (D.text (scmPrefixed "define ") <> (D.text $ unEscape ident))
      $ printCurriedAbs (map unEscape $ NEA.toArray args) expr
  DefineUncurriedFunction ident args expr ->
    printNamedIndentedList (D.text (scmPrefixed "define ") <> (D.text $ unEscape ident))
      $ printNamedIndentedList
          (D.text (scmPrefixed "lambda ") <> printList (D.words $ map (D.text <<< unEscape) args))
          (printChezExpr expr)
  DefineRecordType ident [ field ] ->
    printRecordDefinition
      (unEscape ident)
      (recordTypeName $ unEscape ident)
      (recordTypeCurriedConstructor $ unEscape ident)
      (recordTypePredicate $ unEscape ident)
      [ unEscape field ]
  DefineRecordType ident fields ->
    printRecordDefinition
      (unEscape ident)
      (recordTypeName $ unEscape ident)
      (recordTypeUncurriedConstructor $ unEscape ident)
      (recordTypePredicate $ unEscape ident)
      (map unEscape fields)

printCurriedAbs :: Prim.Array Prim.String -> ChezExpr -> Doc Void
printCurriedAbs args body = Array.foldr foldFn (printChezExpr body) args
  where
  foldFn next bodyOrRest =
    printNamedIndentedList
      (D.words [ D.text $ scmPrefixed "lambda", printList (D.text next) ])
      bodyOrRest

printChezExpr :: forall a. ChezExpr -> Doc a
printChezExpr e = case e of
  Integer (LiteralDigit x) -> D.text x
  Float (LiteralDigit x) -> D.text x
  String x -> D.text x
  Boolean x -> D.text $ if x then "#t" else "#f"
  Identifier x -> D.text $ unEscape x
  List xs -> D.text "(" <> D.words (printChezExpr <$> xs) <> D.text ")"

printRecordDefinition
  :: Prim.String
  -> Prim.String
  -> Prim.String
  -> Prim.String
  -> Prim.Array Prim.String
  -> Doc Void
printRecordDefinition ident name constructor predicate fields =
  printNamedIndentedList defineForm fieldsForm
  where
  defineForm :: Doc Void
  defineForm = D.words
    [ D.text $ scmPrefixed "define-record-type"
    , printList $ D.words $ map D.text [ name, constructor, predicate ]
    ]

  fieldForm :: Prim.String -> Doc Void
  fieldForm field = printList $ D.words
    $ map D.text [ scmPrefixed "immutable", field, recordTypeAccessor ident field ]

  fieldsForm :: Doc Void
  fieldsForm = printList $ D.words $ Array.cons (D.text $ scmPrefixed "fields")
    $ map fieldForm fields

toChezIdent :: Maybe Ident -> Level -> Prim.String
toChezIdent i (Level l) = case i of
  Just (Ident i') -> case i' of
    "$__unused" -> "_"
    _ -> i' <> show l
  Nothing -> "_" <> show l

jsonToChezString :: Prim.String -> Prim.String
jsonToChezString str = unicodeReplace str
  where
  unicodeRegex :: R.Regex
  unicodeRegex = R.Unsafe.unsafeRegex """\\u([A-F\d]{4})""" R.Flags.global

  unicodeReplace :: Prim.String -> Prim.String
  unicodeReplace s = R.replace' unicodeRegex unicodeReplaceMatch s

  unicodeReplaceMatch
    :: Prim.String
    -> Prim.Array (Maybe Prim.String)
    -> Prim.String
  unicodeReplaceMatch _ = case _ of
    [ (Just x) ] -> "\\x" <> x <> ";"
    _ -> unsafeCrashWith "Error matching at unicodeReplaceMatch in jsonToChezString"

--

chezCond :: NonEmptyArray { c :: ChezExpr, e :: ChezExpr } -> Maybe ChezExpr -> ChezExpr
chezCond b o =
  let
    b' :: Prim.Array ChezExpr
    b' = NonEmptyArray.toArray b <#> \{ c, e } -> List [ c, e ]

    o' :: Prim.Array ChezExpr
    o' = Array.fromFoldable o <#> \x -> List [ Identifier $ chezIdent $ scmPrefixed "else", x ]
  in
    List $ [ Identifier $ chezIdent $ scmPrefixed "cond" ] <> b' <> o'

chezUncurriedApplication :: ChezExpr -> Prim.Array ChezExpr -> ChezExpr
chezUncurriedApplication f args = List $ Array.cons f args

chezCurriedApplication :: ChezExpr -> NonEmptyArray ChezExpr -> ChezExpr
chezCurriedApplication f s = NonEmptyArray.foldl1 app $ NonEmptyArray.cons f s

chezCurriedFunction :: NonEmptyArray Prim.String -> ChezExpr -> ChezExpr
chezCurriedFunction a e = Array.foldr lambda e $ NonEmptyArray.toArray a

chezThunk :: ChezExpr -> ChezExpr
chezThunk e = List [ Identifier $ chezIdent $ scmPrefixed "lambda", List [], e ]

chezUnthunk :: ChezExpr -> ChezExpr
chezUnthunk e = List [ e ]

--

app :: ChezExpr -> ChezExpr -> ChezExpr
app f x = List [ f, x ]

define :: Ident -> ChezExpr -> ChezExpr
define i e = List
  [ Identifier $ chezIdent $ scmPrefixed "define", Identifier $ chezIdent $ coerce i, e ]

library :: ModuleName -> Prim.Array Ident -> Prim.Array ChezExpr -> ChezExpr
library moduleName exports bindings =
  List $
    [ Identifier $ chezIdent "library"
    , List [ Identifier $ chezIdent (coerce moduleName), Identifier $ chezIdent "lib" ]
    , List $
        [ Identifier $ chezIdent "export"
        ] <> (Identifier <<< chezIdent <$> coerce exports)
    , List
        [ Identifier $ chezIdent "import"
        , List
            [ Identifier $ chezIdent "prefix"
            , List
                [ Identifier $ chezIdent "chezscheme"
                ]
            , Identifier $ chezIdent libChezSchemePrefix
            ]
        ]
    ] <> bindings

record :: Prim.Array (Prop ChezExpr) -> ChezExpr
record r =
  let
    field :: Prop ChezExpr -> ChezExpr
    field (Prop k v) =
      List
        [ Identifier $ chezIdent $ scmPrefixed "hashtable-set!"
        , Identifier $ chezIdent "$record"
        , String $ Json.stringify $ Json.fromString k
        , v
        ]
  in
    List $
      [ Identifier $ chezIdent $ scmPrefixed "letrec*"
      , List
          [ List
              [ Identifier $ chezIdent "$record"
              , List
                  [ Identifier $ chezIdent $ scmPrefixed "make-hashtable"
                  , Identifier $ chezIdent $ scmPrefixed "string-hash"
                  , Identifier $ chezIdent $ scmPrefixed "string=?"
                  ]
              ]
          ]
      ] <> (field <$> r) <> [ Identifier $ chezIdent "$record" ]

quote :: ChezExpr -> ChezExpr
quote e = app (Identifier $ chezIdent $ scmPrefixed "quote") e

eqQ :: ChezExpr -> ChezExpr -> ChezExpr
eqQ x y = chezUncurriedApplication (Identifier $ chezIdent $ scmPrefixed "eq?") [ x, y ]

lambda :: Prim.String -> ChezExpr -> ChezExpr
lambda a e = List
  [ Identifier $ chezIdent $ scmPrefixed "lambda", List [ Identifier $ chezIdent a ], e ]

vector :: Prim.Array ChezExpr -> ChezExpr
vector = List <<< Array.cons (Identifier $ chezIdent $ scmPrefixed "vector")

recordTypeName :: Prim.String -> Prim.String
recordTypeName i = i <> "$"

recordTypeCurriedConstructor :: Prim.String -> Prim.String
recordTypeCurriedConstructor i = i

recordTypeUncurriedConstructor :: Prim.String -> Prim.String
recordTypeUncurriedConstructor i = i <> "*"

recordTypePredicate :: Prim.String -> Prim.String
recordTypePredicate i = i <> "?"

recordTypeAccessor :: Prim.String -> Prim.String -> Prim.String
recordTypeAccessor i field = i <> "-" <> field

recordAccessor :: ChezExpr -> Prim.String -> Prim.String -> ChezExpr
recordAccessor expr name field =
  chezUncurriedApplication (Identifier $ chezIdent $ recordTypeAccessor name field) [ expr ]
