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
import Dodo (Doc)
import Dodo as D
import Prim as Prim
import PureScript.Backend.Optimizer.CoreFn (Ident(..), ModuleName(..), Prop(..))
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
  { identifiers :: NonEmptyArray Prim.String
  , version :: Prim.Array LibraryVersion
  }

newtype LibraryVersion = LibraryVersion Prim.Int

derive instance Newtype LibraryVersion _

data ChezExport
  = ExportIdentifier Prim.String
  | ExportRename (Prim.Array { original :: Prim.String, rename :: Prim.String })

type LibraryReference =
  { identifiers :: NonEmptyArray Prim.String
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
  | ImportOnly ChezImportSet (Prim.Array Prim.String)
  | ImportExcept ChezImportSet (Prim.Array Prim.String)
  | ImportPrefix ChezImportSet Prim.String
  | ImportRename ChezImportSet (Prim.Array { original :: Prim.String, rename :: Prim.String })

type LibraryBody =
  { definitions :: Prim.Array ChezDefinition
  , expressions :: Prim.Array ChezExpr
  }

data ChezDefinition
  = DefineValue Prim.String ChezExpr
  | DefineCurriedFunction Prim.String (NonEmptyArray Prim.String) ChezExpr
  | DefineUncurriedFunction Prim.String (Prim.Array Prim.String) ChezExpr
  | DefineRecordType Prim.String (Prim.Array Prim.String)

newtype LiteralDigit = LiteralDigit Prim.String

derive instance Newtype LiteralDigit _
derive newtype instance Eq LiteralDigit
derive newtype instance Ord LiteralDigit

data ChezExpr
  = Integer LiteralDigit
  | Float LiteralDigit
  | String Prim.String
  | Boolean Prim.Boolean
  | Identifier Prim.String
  | List (Prim.Array ChezExpr)

definitionIdentifiers :: ChezDefinition -> Prim.Array Prim.String
definitionIdentifiers (DefineValue i _) = [ i ]
definitionIdentifiers (DefineCurriedFunction i _ _) = [ i ]
definitionIdentifiers (DefineUncurriedFunction i _ _) = [ i ]
definitionIdentifiers (DefineRecordType i _) =
  [ recordTypeUncurriedConstructor i
  , recordTypePredicate i
  ]

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
    [ D.words $ map D.text identifiers
    , case version of
        [] -> mempty
        _ -> printList $ D.words $ map printLibraryVersion version
    ]

printLibraryVersion :: LibraryVersion -> Doc Void
printLibraryVersion = D.text <<< show <<< un LibraryVersion

printExport :: ChezExport -> Doc Void
printExport = case _ of
  ExportIdentifier s -> D.text s
  ExportRename arr ->
    printNamedList "rename"
      $ D.words
      $ map (\r -> printList $ D.words [ D.text r.original, D.text r.rename ]) arr

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
      [ printImportSet impSet, D.words $ map D.text identifiers ]
  ImportExcept impSet identifiers ->
    printNamedList "except" $ D.words
      [ printImportSet impSet, D.words $ map D.text identifiers ]
  ImportPrefix impSet rename ->
    printNamedList "prefix" $ D.words [ printImportSet impSet, D.text rename ]
  ImportRename impSet identifiers ->
    printNamedList "rename" $ D.words
      [ printImportSet impSet
      , D.words $ map (\r -> printList $ D.words $ map D.text [ r.original, r.rename ])
          identifiers
      ]

printLibraryReference :: LibraryReference -> Doc Void
printLibraryReference lib = do
  let
    printListFn
      | Array.elem (NEA.head lib.identifiers)
          [ "for", "library", "only", "except", "prefix", "rename" ] =
          printNamedList "library"
      | otherwise =
          printList
  printListFn $ D.words
    [ D.words $ map D.text lib.identifiers
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
    printNamedIndentedList (D.words [ D.text "scm:define", D.text ident ])
      $ printChezExpr expr
  DefineCurriedFunction ident args expr ->
    printNamedIndentedList (D.text "scm:define " <> D.text ident)
      $ printCurriedApp (NEA.toArray args) expr
  DefineUncurriedFunction ident args expr ->
    printNamedIndentedList (D.text "scm:define " <> D.text ident)
      $ printNamedIndentedList
          (D.text "scm:lambda " <> printList (D.words $ map D.text args))
          (printChezExpr expr)
  DefineRecordType ident fields ->
    printNamedIndentedList
      ( D.words
          [ D.text "scm:define-record-type"
          , printList
              $ D.words
              $ map D.text
                  [ recordTypeName ident
                  , recordTypeUncurriedConstructor ident
                  , recordTypePredicate ident
                  ]
          ]
      )
      $ printList
      $ D.words
      $ map D.text
      $ Array.cons "scm:fields" fields

printCurriedApp :: Prim.Array Prim.String -> ChezExpr -> Doc Void
printCurriedApp args body = case Array.uncons args of
  Nothing -> printChezExpr body
  Just { head, tail } ->
    printNamedIndentedList
      (D.text "scm:lambda " <> printList (D.text head))
      (printCurriedApp tail body)

printChezExpr :: forall a. ChezExpr -> Doc a
printChezExpr e = case e of
  Integer (LiteralDigit x) -> D.text x
  Float (LiteralDigit x) -> D.text x
  String x -> D.text x
  Boolean x -> D.text $ if x then "#t" else "#f"
  Identifier x -> D.text x
  List xs -> D.text "(" <> D.words (printChezExpr <$> xs) <> D.text ")"

toChezIdent :: Maybe Ident -> Level -> Prim.String
toChezIdent i (Level l) = case i of
  Just (Ident i') -> i' <> show l
  Nothing -> "_" <> show l

--

chezCond :: NonEmptyArray { c :: ChezExpr, e :: ChezExpr } -> Maybe ChezExpr -> ChezExpr
chezCond b o =
  let
    b' :: Prim.Array ChezExpr
    b' = NonEmptyArray.toArray b <#> \{ c, e } -> List [ c, e ]

    o' :: Prim.Array ChezExpr
    o' = Array.fromFoldable o <#> \x -> List [ Identifier "scm:else", x ]
  in
    List $ [ Identifier "scm:cond" ] <> b' <> o'

chezCurriedApplication :: ChezExpr -> NonEmptyArray ChezExpr -> ChezExpr
chezCurriedApplication f s = NonEmptyArray.foldl1 app $ NonEmptyArray.cons f s

chezCurriedFunction :: NonEmptyArray Prim.String -> ChezExpr -> ChezExpr
chezCurriedFunction a e = Array.foldr lambda e $ NonEmptyArray.toArray a

chezLet :: Prim.String -> ChezExpr -> ChezExpr -> ChezExpr
chezLet i v e = List [ Identifier "scm:letrec*", List [ List [ Identifier i, v ] ], e ]

--

app :: ChezExpr -> ChezExpr -> ChezExpr
app f x = List [ f, x ]

define :: Ident -> ChezExpr -> ChezExpr
define i e = List [ Identifier "scm:define", Identifier $ coerce i, e ]

library :: ModuleName -> Prim.Array Ident -> Prim.Array ChezExpr -> ChezExpr
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
            , Identifier "scm:"
            ]
        ]
    ] <> bindings

record :: Prim.Array (Prop ChezExpr) -> ChezExpr
record r =
  let
    field :: Prop ChezExpr -> ChezExpr
    field (Prop k v) =
      List
        [ Identifier "scm:hashtable-set!"
        , Identifier "$record"
        , String $ Json.stringify $ Json.fromString k
        , v
        ]
  in
    List $
      [ Identifier "scm:letrec*"
      , List
          [ List
              [ Identifier "$record"
              , List
                  [ Identifier "scm:make-hashtable"
                  , Identifier "scm:string-hash"
                  , Identifier "scm:string=?"
                  ]
              ]
          ]
      ] <> (field <$> r) <> [ Identifier "$record" ]

lambda :: Prim.String -> ChezExpr -> ChezExpr
lambda a e = List [ Identifier "scm:lambda", List [ Identifier a ], e ]

vector :: Prim.Array ChezExpr -> ChezExpr
vector = List <<< Array.cons (Identifier "scm:vector")

recordTypeName :: Prim.String -> Prim.String
recordTypeName i = i <> "$"

recordTypeUncurriedConstructor :: Prim.String -> Prim.String
recordTypeUncurriedConstructor i = i <> "*"

recordTypePredicate :: Prim.String -> Prim.String
recordTypePredicate i = i <> "?"

recordAccessor :: ChezExpr -> Prim.Int -> ChezExpr
recordAccessor expr offset =
  chezLet "$record" expr
    $ List
        [ List
            [ Identifier "scm:record-accessor"
            , List
                [ Identifier "scm:record-rtd"
                , Identifier "$record"
                ]
            , Integer $ LiteralDigit $ show offset
            ]
        , Identifier "$record"
        ]
