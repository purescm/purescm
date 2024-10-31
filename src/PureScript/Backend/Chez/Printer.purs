module PureScript.Backend.Chez.Printer
  ( printLibrary
  ) where

import Prelude

import Control.Alternative as Alternative
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Dodo (Doc)
import Dodo as D
import PureScript.Backend.Chez.Constants (rtPrefixed, scmPrefixed)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr(..), ChezImport(..), ChezImportLevel(..), ChezImportSet(..), ChezLibrary, LibraryBody, LibraryName, LibraryReference, LibraryVersion(..), LiteralDigit(..), SubVersionReference(..), VersionReference(..), lazyRefName, recordTypeAccessor, recordTypeCurriedConstructor, recordTypeName, recordTypePredicate, recordTypeUncurriedConstructor)
import PureScript.Backend.Optimizer.CoreFn (ModuleName(..))

printWrap :: Doc Void -> Doc Void -> Doc Void -> Doc Void
printWrap l r x = l <> x <> r

printList :: Doc Void -> Doc Void
printList = printWrap (D.text "(") (D.text ")")

printBrackets :: Doc Void -> Doc Void
printBrackets = printWrap (D.text "[") (D.text "]")

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

escapeIdentifiers :: ChezLibrary -> ChezLibrary
escapeIdentifiers lib = lib
  { name = lib.name { identifiers = map escapeIdent lib.name.identifiers }
  , exports = map escapeExport lib.exports
  , imports = map escapeImport lib.imports
  , body =
      { definitions: escapeDefinition <$> lib.body.definitions
      , expressions: escapeExpr <$> lib.body.expressions
      }
  }
  where
  escapeIdent = String.replaceAll (Pattern "'") (Replacement "$p")
  escapeExport = case _ of
    ExportIdentifier x -> ExportIdentifier $ escapeIdent x
    ExportRename arr -> ExportRename $ arr <#> \r ->
      { original: escapeIdent r.original, rename: escapeIdent r.rename }

  escapeImport = case _ of
    ImportSet set -> ImportSet $ escapeImportSet set
    ImportFor set lvls -> ImportFor (escapeImportSet set) lvls

  escapeImportSet = case _ of
    ImportLibrary libRef ->
      ImportLibrary $ escapeLibRef libRef
    ImportOnly set idents ->
      ImportOnly (escapeImportSet set) $ map escapeIdent idents
    ImportExcept set idents ->
      ImportExcept (escapeImportSet set) $ map escapeIdent idents
    ImportPrefix set ident ->
      ImportPrefix (escapeImportSet set) $ escapeIdent ident
    ImportRename set renames ->
      ImportRename (escapeImportSet set) $ renames <#> \r ->
        { original: escapeIdent r.original, rename: escapeIdent r.rename }

  escapeLibRef libRef = libRef { identifiers = map escapeIdent libRef.identifiers }

  escapeDefinition = case _ of
    Define i expr -> Define (escapeIdent i) $ escapeExpr expr
    DefineLazy i moduleName expr -> DefineLazy (escapeIdent i) moduleName $ escapeExpr expr
    DefineRecordType i fields -> DefineRecordType (escapeIdent i) $ map escapeIdent fields

  escapeExpr = case _ of
    Identifier i -> Identifier $ escapeIdent i
    List exprs -> List $ map escapeExpr exprs
    Cond b o -> Cond (map (bimap escapeExpr escapeExpr) b) (escapeExpr o)
    Let recursive bindings expr ->
      Let recursive (map (bimap escapeIdent escapeExpr) bindings) $ escapeExpr expr
    Lambda args expr -> Lambda (map escapeIdent args) $ escapeExpr expr
    x@(Integer _) -> x
    x@(Float _) -> x
    x@(StringExpr _) -> x
    x@(Char _) -> x
    x@(Bool _) -> x

printLibrary :: ChezLibrary -> Doc Void
printLibrary = escapeIdentifiers >>> printLibrary'

printLibrary' :: ChezLibrary -> Doc Void
printLibrary' lib =
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
  Define ident expr ->
    printNamedIndentedList (D.words [ D.text $ scmPrefixed "define", D.text ident ])
      $ printChezExpr expr
  DefineLazy ident (ModuleName m) expr -> do
    let origIdentName = Json.stringify $ Json.fromString ident
    let moduleName = Json.stringify $ Json.fromString m
    printNamedIndentedList
      ( D.words
          [ D.text $ rtPrefixed "define-lazy"
          , D.text (lazyRefName ident)
          , D.text origIdentName
          , D.text moduleName
          ]
      )
      $ printChezExpr expr
  DefineRecordType ident [ field ] ->
    printRecordDefinition
      ident
      (recordTypeName ident)
      (recordTypeCurriedConstructor ident)
      (recordTypePredicate ident)
      [ field ]
  DefineRecordType ident fields ->
    printRecordDefinition
      ident
      (recordTypeName ident)
      (recordTypeUncurriedConstructor ident)
      (recordTypePredicate ident)
      fields

printChezExpr :: ChezExpr -> Doc Void
printChezExpr e = case e of
  Integer (LiteralDigit x) -> D.text x
  Float (LiteralDigit x) -> D.text x
  Char x -> D.text x
  StringExpr x -> D.text x
  Bool x -> D.text $ if x then "#t" else "#f"
  Identifier x -> D.text x
  List xs -> printList $ D.words (printChezExpr <$> xs)
  Cond branches fallback -> printCond branches fallback
  Let recursive bindings' expr -> printLet recursive bindings' expr
  Lambda args expr -> printLambda args expr

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

printLet :: Boolean -> NonEmptyArray (Tuple String ChezExpr) -> ChezExpr -> Doc Void
printLet recursive bindings' expr = do
  let
    multiLetKeyword
      | recursive = D.text $ scmPrefixed "letrec*"
      | otherwise = D.text $ scmPrefixed "let*"
    singleLetKeyword
      | recursive = D.text $ scmPrefixed "letrec"
      | otherwise = D.text $ scmPrefixed "let"
    printBinding (Tuple b e) = printBrackets $ D.words [ D.text b, printChezExpr e ]

  case NEA.uncons bindings' of
    { head, tail: [] } ->
      D.lines
        [ D.text "(" <> D.words [ singleLetKeyword, printList $ printBinding head ]
        , D.indent $ printChezExpr expr <> D.text ")"
        ]
    { head, tail } ->
      D.lines
        [ D.text "(" <> multiLetKeyword
        , D.indent $ D.lines
            [ D.text "(" <> printBinding head
            , (D.lines $ map (append D.space <<< printBinding) tail) <> D.text ")"
            , D.indent $ printChezExpr expr <> D.text ")"
            ]
        ]

printLambda :: Array String -> ChezExpr -> Doc Void
printLambda args expr =
  printNamedIndentedList
    (D.words [ D.text $ scmPrefixed "lambda", printList $ D.words $ map D.text args ])
    $ printChezExpr expr

printCond :: NonEmptyArray (Tuple ChezExpr ChezExpr) -> ChezExpr -> Doc Void
printCond branches fallback = do
  let
    b' :: Array (Doc Void)
    b' = NonEmptyArray.toArray branches <#> \(Tuple cond expr) ->
      printBrackets $ D.words [ printChezExpr cond, printChezExpr expr ]

    o' :: Doc Void
    o' = printBrackets $ D.words [ D.text $ scmPrefixed "else", printChezExpr fallback ]

  printNamedIndentedList (D.text $ scmPrefixed "cond")
    $ D.lines
    $ Array.snoc b' o'
