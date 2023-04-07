module PureScript.Backend.Chez.Convert where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char (toCharCode)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.String.Regex as R
import Data.String.Regex.Flags as R.Flags
import Data.String.Regex.Unsafe as R.Unsafe
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple as Tuple
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Chez.Constants (libChezSchemePrefix, moduleForeign, moduleLib, runtimePrefix, scmPrefixed)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary, recordTypeAccessor, recordTypeCurriedConstructor, recordTypePredicate, recordTypeUncurriedConstructor)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))
import Safe.Coerce (coerce)

type CodegenEnv =
  { currentModule :: ModuleName
  }

codegenModule :: BackendModule -> ChezLibrary
codegenModule { name, bindings, imports, foreign: foreign_ } =
  let
    codegenEnv :: CodegenEnv
    codegenEnv = { currentModule: name }

    definitions :: Array ChezDefinition
    definitions = Array.concat $
      codegenTopLevelBindingGroup codegenEnv <$> bindings

    exports' :: Array ChezExport
    exports' = map ExportIdentifier
      $ Array.sort
      $ Array.concatMap definitionIdentifiers definitions
          <> map coerce (Array.fromFoldable foreign_)

    pursImports :: Array ChezImport
    pursImports = Array.fromFoldable imports <#> \importedModule ->
      ImportSet $ ImportPrefix
        ( ImportLibrary
            { identifiers: NEA.cons' (coerce importedModule) [ moduleLib ], version: Nothing }
        )
        (coerce importedModule <> ".")

    foreignImport :: Array ChezImport
    foreignImport
      | Set.isEmpty foreign_ = []
      | otherwise =
          [ ImportSet $
              ImportLibrary
                { identifiers: NEA.cons' (coerce name) [ moduleForeign ], version: Nothing }
          ]
  in
    { "#!r6rs": true
    , "#!chezscheme": true
    , name:
        { identifiers: NEA.cons' (coerce name) [ moduleLib ]
        , version: []
        }
    , imports:
        [ ImportSet $ ImportPrefix
            (ImportLibrary { identifiers: NEA.singleton "chezscheme", version: Nothing })
            libChezSchemePrefix
        , ImportSet $ ImportPrefix
            ( ImportLibrary
                { identifiers: NEA.cons' "_Chez_Runtime" [ moduleLib ], version: Nothing }
            )
            runtimePrefix
        ] <> pursImports <> foreignImport
    , exports: exports'
    , body: { definitions, expressions: [] }
    }

definitionIdentifiers :: ChezDefinition -> Array String
definitionIdentifiers = case _ of
  DefineValue i _ -> [ i ]
  DefineCurriedFunction i _ _ -> [ i ]
  DefineUncurriedFunction i _ _ -> [ i ]
  DefineRecordType i [ x ] ->
    [ recordTypeCurriedConstructor i
    , recordTypePredicate i
    , recordTypeAccessor i x
    ]
  DefineRecordType i fields ->
    [ recordTypeUncurriedConstructor i
    , recordTypePredicate i
    ] <> map (recordTypeAccessor i) fields

flattenQualified :: ModuleName -> Qualified Ident -> String
flattenQualified _ (Qualified Nothing (Ident i)) = i
flattenQualified currentModule (Qualified (Just m) (Ident i))
  | currentModule == m = i
  | otherwise = coerce m <> "." <> i

toChezIdent :: Maybe Ident -> Level -> String
toChezIdent i (Level l) = case i of
  Just (Ident i') -> case i' of
    "$__unused" -> "_"
    _ -> i' <> show l
  Nothing -> "_" <> show l

codegenTopLevelBindingGroup
  :: CodegenEnv
  -> BackendBindingGroup Ident NeutralExpr
  -> Array ChezDefinition
codegenTopLevelBindingGroup codegenEnv { recursive, bindings }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings =
      [ DefineValue "rtlbg" $ S.Identifier "recursive-top-level-binding-group" ]
  | otherwise =
      Array.concatMap (codegenTopLevelBinding codegenEnv) bindings

codegenTopLevelBinding
  :: CodegenEnv
  -> Tuple Ident NeutralExpr
  -> Array ChezDefinition
codegenTopLevelBinding codegenEnv (Tuple (Ident i) n) =
  case unwrap n of
    Abs a e ->
      [ DefineCurriedFunction
          i
          (uncurry toChezIdent <$> a)
          (codegenExpr codegenEnv e)
      ]
    UncurriedAbs a e ->
      [ DefineUncurriedFunction
          i
          (uncurry toChezIdent <$> a)
          (codegenExpr codegenEnv e)
      ]
    CtorDef _ _ _ ss ->
      case NonEmptyArray.fromArray ss of
        Nothing ->
          [ DefineValue i (S.quote $ S.Identifier i)
          , DefineUncurriedFunction
              (S.recordTypePredicate i)
              [ "v" ]
              $ S.eqQ (S.quote $ S.Identifier i) (S.Identifier "v")
          ]
        Just xs
          | NEA.length xs == 1 ->
              [ DefineRecordType i ss ]
          | otherwise ->
              [ DefineRecordType i ss
              , DefineCurriedFunction i xs
                  $ S.runUncurriedFn
                      (S.Identifier $ S.recordTypeUncurriedConstructor i)
                      (map S.Identifier ss)
              ]
    _ ->
      [ DefineValue i $ codegenExpr codegenEnv n ]

codegenExpr :: CodegenEnv -> NeutralExpr -> ChezExpr
codegenExpr codegenEnv@{ currentModule } s = case unwrap s of
  Var qi ->
    S.Identifier $ flattenQualified currentModule qi
  Local i l ->
    S.Identifier $ coerce $ toChezIdent i l
  Lit l ->
    codegenLiteral codegenEnv l
  App f p ->
    S.runCurriedFn (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    S.mkCurriedFn (uncurry toChezIdent <$> a) (codegenExpr codegenEnv e)
  UncurriedApp f p ->
    S.runUncurriedFn (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedAbs a e ->
    S.mkUncurriedFn (uncurry toChezIdent <$> a) (codegenExpr codegenEnv e)
  UncurriedEffectApp f p ->
    S.thunk $ S.runUncurriedFn (codegenExpr codegenEnv f)
      (codegenExpr codegenEnv <$> p)
  UncurriedEffectAbs a e ->
    S.mkUncurriedFn (uncurry toChezIdent <$> a)
      (codegenChain effectChainMode codegenEnv e)

  Accessor e (GetProp i) ->
    S.runUncurriedFn
      (S.Identifier $ scmPrefixed "hashtable-ref")
      [ codegenExpr codegenEnv e
      , S.StringExpr $ Json.stringify $ Json.fromString i
      , S.Bool false
      ]
  Accessor e (GetIndex i) ->
    S.runUncurriedFn
      (S.Identifier $ scmPrefixed "vector-ref")
      [ codegenExpr codegenEnv e, S.Integer $ wrap $ show i ]
  Accessor e (GetCtorField qi _ _ _ field _) ->
    S.recordAccessor (codegenExpr codegenEnv e) (flattenQualified currentModule qi) field
  Update _ _ ->
    S.Identifier "object-update"

  CtorSaturated qi _ _ _ xs ->
    S.runUncurriedFn
      (S.Identifier $ S.recordTypeUncurriedConstructor $ flattenQualified currentModule qi)
      (map (codegenExpr codegenEnv <<< Tuple.snd) xs)
  CtorDef _ _ _ _ ->
    unsafeCrashWith "codegenExpr:CtorDef - handled by codegenTopLevelBinding!"

  LetRec _ _ _ ->
    S.Identifier "let-rec"
  Let _ _ _ _ ->
    codegenPureChain codegenEnv s
  Branch b o -> do
    let
      goPair :: Pair NeutralExpr -> Tuple ChezExpr ChezExpr
      goPair (Pair c e) = Tuple (codegenExpr codegenEnv c) (codegenExpr codegenEnv e)
    S.Cond (goPair <$> b) (codegenExpr codegenEnv <$> o)

  EffectBind _ _ _ _ ->
    codegenEffectChain codegenEnv s
  EffectPure _ ->
    codegenEffectChain codegenEnv s
  EffectDefer _ ->
    codegenEffectChain codegenEnv s

  PrimOp o ->
    codegenPrimOp codegenEnv o
  PrimEffect _ ->
    S.Identifier "prim-effect"
  PrimUndefined ->
    S.Identifier "prim-undefined"

  Fail i ->
    -- Note: This can be improved by using `error`, but it requires
    -- that we track where exactly this `Fail` is defined. We can
    -- make use of the `codegenEnv` for this.
    S.List
      [ S.Identifier $ scmPrefixed "raise"
      , S.List
          [ S.Identifier $ scmPrefixed "condition"
          , S.List [ S.Identifier $ scmPrefixed "make-error" ]
          , S.List
              [ S.Identifier $ scmPrefixed "make-message-condition"
              , S.StringExpr $ Json.stringify $ Json.fromString i
              ]
          ]
      ]

codegenLiteral :: CodegenEnv -> Literal NeutralExpr -> ChezExpr
codegenLiteral codegenEnv = case _ of
  LitInt i -> S.Integer $ wrap $ show i
  LitNumber n -> S.Float $ wrap $ show n
  LitString s -> S.StringExpr $ jsonToChezString $ Json.stringify $ Json.fromString s
  LitChar c -> codegenChar c
  LitBoolean b -> S.Identifier $ if b then "#t" else "#f"
  LitArray a -> S.vector $ codegenExpr codegenEnv <$> a
  LitRecord r -> S.record $ (map $ codegenExpr codegenEnv) <$> r

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

-- > In addition to the standard named characters 
-- > #\alarm, #\backspace, #\delete, #\esc, #\linefeed, #\newline, #\page, #\return, #\space, and #\tab, 
-- > Chez Scheme recognizes #\bel, #\ls, #\nel, #\nul, #\rubout, and #\vt (or #\vtab).
--
-- Source: https://cisco.github.io/ChezScheme/csug9.5/intro.html#./intro:h1, 6th paragraph
codegenChar :: Char -> ChezExpr
codegenChar c = S.Char $ append """#\""" $ escapeChar $ toCharCode c
  where
  escapeChar code
    | code == toCharCode '\x0000' = "nul"
    | code == toCharCode '\x0007' = "alarm" -- bel
    | code == toCharCode '\x0008' = "backspace"
    | code == toCharCode '\t' = "tab"
    | code == toCharCode '\n' = "linefeed" -- nel/newline; per R6Rs, newline is deprecated
    | code == toCharCode '\x000B' = "vtab"
    | code == toCharCode '\x000C' = "page"
    | code == toCharCode '\r' = "return"
    | code == toCharCode '\x001B' = "esc"
    | code == toCharCode ' ' = "space"
    | code == toCharCode '\x007F' = "delete" -- rubout
    | code == toCharCode '\x2028' = "ls"
    | code < 20 || code > 127 = "x" <> (padLeft "0" 4 $ Int.toStringAs Int.hexadecimal code)
    | otherwise = CodeUnits.singleton c

  padLeft char i s = power char (i - String.length s) <> s

type ChainMode = { effect :: Boolean }

pureChainMode :: ChainMode
pureChainMode = { effect: false }

effectChainMode :: ChainMode
effectChainMode = { effect: true }

codegenPureChain :: CodegenEnv -> NeutralExpr -> ChezExpr
codegenPureChain codegenEnv = codegenChain pureChainMode codegenEnv

codegenEffectChain :: CodegenEnv -> NeutralExpr -> ChezExpr
codegenEffectChain codegenEnv = S.thunk <<< codegenChain effectChainMode codegenEnv

codegenChain :: ChainMode -> CodegenEnv -> NeutralExpr -> ChezExpr
codegenChain chainMode codegenEnv = collect []
  where
  recursive = false

  finish :: Boolean -> Array _ -> NeutralExpr -> ChezExpr
  finish shouldUnthunk bindings expression = do
    let
      maybeUnthunk :: ChezExpr -> ChezExpr
      maybeUnthunk = if shouldUnthunk then S.unthunk else identity
    case NEA.fromArray bindings of
      Nothing -> maybeUnthunk $ codegenExpr codegenEnv expression
      Just bindings' -> S.Let recursive bindings' $ maybeUnthunk $ codegenExpr codegenEnv expression

  collect :: Array _ -> NeutralExpr -> ChezExpr
  collect bindings expression = case unwrap expression of
    Let i l v e' ->
      collect (Array.snoc bindings $ Tuple (toChezIdent i l) (codegenExpr codegenEnv v)) e'
    EffectPure e' | chainMode.effect ->
      finish false bindings e'
    EffectBind i l v e' | chainMode.effect ->
      collect
        (Array.snoc bindings $ Tuple (toChezIdent i l) (S.unthunk $ codegenExpr codegenEnv v))
        e'
    EffectDefer e' | chainMode.effect ->
      collect bindings e'
    _ ->
      finish chainMode.effect bindings expression

codegenPrimOp :: CodegenEnv -> BackendOperator NeutralExpr -> ChezExpr
codegenPrimOp codegenEnv@{ currentModule } = case _ of
  Op1 o x -> do
    let
      x' = codegenExpr codegenEnv x
    case o of
      OpBooleanNot ->
        S.List [ S.Identifier $ scmPrefixed "not", x' ]
      OpIntBitNot ->
        S.List [ S.Identifier $ scmPrefixed "fxlognot", x' ]
      OpIntNegate ->
        S.List [ S.Identifier $ scmPrefixed "fx-", x' ]
      OpNumberNegate ->
        S.List [ S.Identifier $ scmPrefixed "fl-", x' ]
      OpArrayLength ->
        S.List [ S.Identifier $ scmPrefixed "vector-length", x' ]
      OpIsTag qi ->
        S.app
          (S.Identifier $ S.recordTypePredicate $ flattenQualified currentModule qi)
          x'
  Op2 o x y ->
    let
      x' = codegenExpr codegenEnv x
      y' = codegenExpr codegenEnv y

      opFixNum = case _ of
        OpAdd -> S.Identifier $ scmPrefixed "fx+"
        OpSubtract -> S.Identifier $ scmPrefixed "fx-"
        OpMultiply -> S.Identifier $ scmPrefixed "fx*"
        OpDivide -> S.Identifier $ scmPrefixed "fx/"

      opFloNum = case _ of
        OpAdd -> S.Identifier $ scmPrefixed "fl+"
        OpSubtract -> S.Identifier $ scmPrefixed "fl-"
        OpMultiply -> S.Identifier $ scmPrefixed "fl*"
        OpDivide -> S.Identifier $ scmPrefixed "fl/"

      makeComparison :: String -> BackendOperatorOrd -> ChezExpr
      makeComparison tyName = do
        let
          comparisonExpression :: String -> ChezExpr
          comparisonExpression opName =
            S.List [ S.Identifier $ Array.fold [ scmPrefixed tyName, opName ], x', y' ]
        case _ of
          OpEq -> comparisonExpression "=?"
          OpNotEq -> S.List [ S.Identifier $ scmPrefixed "not", comparisonExpression "=?" ]
          OpGt -> comparisonExpression ">?"
          OpGte -> comparisonExpression ">=?"
          OpLt -> comparisonExpression "<?"
          OpLte -> comparisonExpression "<=?"
    in
      case o of
        OpArrayIndex ->
          S.List [ S.Identifier $ scmPrefixed "vector-ref", x', y' ]
        OpBooleanAnd ->
          S.List [ S.Identifier $ scmPrefixed "and", x', y' ]
        OpBooleanOr ->
          S.List [ S.Identifier $ scmPrefixed "or", x', y' ]
        OpBooleanOrd o' -> do
          -- Gt, Gte, Lt, Lte are defined under rt:
          let
            comparisonExpression :: String -> String -> ChezExpr
            comparisonExpression prefix opName =
              S.List [ S.Identifier $ Array.fold [ prefix, "boolean", opName ], x', y' ]
          case o' of
            OpEq -> comparisonExpression libChezSchemePrefix "=?"
            OpNotEq -> S.List
              [ S.Identifier $ scmPrefixed "not", comparisonExpression libChezSchemePrefix "=?" ]
            OpGt -> comparisonExpression runtimePrefix ">?"
            OpGte -> comparisonExpression runtimePrefix ">=?"
            OpLt -> comparisonExpression runtimePrefix "<?"
            OpLte -> comparisonExpression runtimePrefix "<=?"
        OpCharOrd o' ->
          makeComparison "char" o'
        OpIntBitAnd ->
          S.List [ S.Identifier $ scmPrefixed "fxlogand", x', y' ]
        OpIntBitOr ->
          S.List [ S.Identifier $ scmPrefixed "fxlogor", x', y' ]
        OpIntBitShiftLeft ->
          -- shift-left-logical = shift-left-arithmetic
          S.List [ S.Identifier $ scmPrefixed "fxsll", x', y' ]
        OpIntBitShiftRight ->
          -- shift-right-arithmetic preserves signs
          S.List [ S.Identifier $ scmPrefixed "fxsra", x', y' ]
        OpIntBitXor ->
          S.List [ S.Identifier $ scmPrefixed "fxlogxor", x', y' ]
        OpIntBitZeroFillShiftRight ->
          -- shift-right-logical follows the invariant:
          --
          -- (= (most-positive-fixnum) (fxsrl -1 1))
          --
          -- or:
          --
          -- top == zshr (-1) 1
          S.List [ S.Identifier $ scmPrefixed "fxsrl", x', y' ]
        OpIntNum o' ->
          S.List [ opFixNum o', x', y' ]
        OpIntOrd o' ->
          makeComparison "fx" o'
        OpNumberNum o' ->
          S.List [ opFloNum o', x', y' ]
        OpNumberOrd o' ->
          makeComparison "fl" o'
        OpStringAppend ->
          S.List [ S.Identifier $ scmPrefixed "string-append", x', y' ]
        OpStringOrd o' ->
          makeComparison "string" o'
