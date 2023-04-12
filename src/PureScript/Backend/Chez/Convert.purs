module PureScript.Backend.Chez.Convert where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Char (toCharCode)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple as Tuple
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Chez.Constants (libChezSchemePrefix, moduleForeign, moduleLib, runtimePrefix, scmPrefixed, undefinedSymbol)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Chez.Syntax.Common as C
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))
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
  Define i _ -> [ i ]
  DefineRecordType i [ x ] ->
    [ C.recordTypeCurriedConstructor i
    , C.recordTypePredicate i
    , C.recordTypeAccessor i x
    ]
  DefineRecordType i fields ->
    [ C.recordTypeUncurriedConstructor i
    , C.recordTypePredicate i
    ] <> map (C.recordTypeAccessor i) fields

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
codegenTopLevelBindingGroup codegenEnv { bindings } =
  Array.concatMap (codegenTopLevelBinding codegenEnv) bindings

codegenTopLevelBinding
  :: CodegenEnv
  -> Tuple Ident NeutralExpr
  -> Array ChezDefinition
codegenTopLevelBinding codegenEnv (Tuple (Ident i) n) =
  case unwrap n of
    CtorDef _ _ _ ss ->
      case NonEmptyArray.fromArray ss of
        Nothing ->
          [ Define i (C.quote $ S.Identifier i)
          , Define (C.recordTypePredicate i)
              $ C.mkUncurriedFn [ "v" ]
              $ C.eqQ (C.quote $ S.Identifier i) (S.Identifier "v")
          ]
        Just xs
          | NEA.length xs == 1 ->
              [ DefineRecordType i ss ]
          | otherwise ->
              [ DefineRecordType i ss
              , Define i $ C.mkCurriedFn xs
                  $ C.runUncurriedFn
                      (S.Identifier $ C.recordTypeUncurriedConstructor i)
                      (map S.Identifier ss)
              ]
    _ ->
      [ Define i $ codegenExpr codegenEnv n ]

codegenExpr :: CodegenEnv -> NeutralExpr -> ChezExpr
codegenExpr codegenEnv@{ currentModule } s = case unwrap s of
  Var qi ->
    S.Identifier $ flattenQualified currentModule qi
  Local i l ->
    S.Identifier $ coerce $ toChezIdent i l
  Lit l ->
    codegenLiteral codegenEnv l
  App f p ->
    C.runCurriedFn (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    C.mkCurriedFn (uncurry toChezIdent <$> a) (codegenExpr codegenEnv e)
  UncurriedApp f p ->
    C.runUncurriedFn (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedAbs a e ->
    C.mkUncurriedFn (uncurry toChezIdent <$> a) (codegenExpr codegenEnv e)
  UncurriedEffectApp f p ->
    C.thunk $ C.runUncurriedFn (codegenExpr codegenEnv f)
      (codegenExpr codegenEnv <$> p)
  UncurriedEffectAbs a e ->
    C.mkUncurriedFn (uncurry toChezIdent <$> a)
      (codegenChain effectChainMode codegenEnv e)

  Accessor e (GetProp i) ->
    C.runUncurriedFn
      (S.Identifier $ scmPrefixed "hashtable-ref")
      [ codegenExpr codegenEnv e
      , S.StringExpr $ wrap i
      , S.Bool false
      ]
  Accessor e (GetIndex i) ->
    C.runUncurriedFn
      (S.Identifier $ scmPrefixed "vector-ref")
      [ codegenExpr codegenEnv e, S.Integer $ wrap $ show i ]
  Accessor e (GetCtorField qi _ _ _ field _) ->
    C.recordAccessor (codegenExpr codegenEnv e) (flattenQualified currentModule qi) field
  Update _ _ ->
    S.Identifier "object-update"

  CtorSaturated qi _ _ _ xs ->
    C.runUncurriedFn
      (S.Identifier $ C.recordTypeUncurriedConstructor $ flattenQualified currentModule qi)
      (map (codegenExpr codegenEnv <<< Tuple.snd) xs)
  CtorDef _ _ _ _ ->
    unsafeCrashWith "codegenExpr:CtorDef - handled by codegenTopLevelBinding!"
  LetRec lvl bindings expr ->
    S.Let true (map (bimap (flip toChezIdent lvl <<< Just) (codegenExpr codegenEnv)) bindings) $
      codegenExpr codegenEnv expr
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
  PrimEffect _ ->
    codegenEffectChain codegenEnv s

  PrimOp o ->
    codegenPrimOp codegenEnv o
  PrimUndefined ->
    C.app (S.Identifier $ scmPrefixed "gensym") (S.StringExpr $ wrap undefinedSymbol)

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
              , S.StringExpr $ wrap i
              ]
          ]
      ]

codegenLiteral :: CodegenEnv -> Literal NeutralExpr -> ChezExpr
codegenLiteral codegenEnv = case _ of
  LitInt i -> S.Integer $ wrap $ show i
  LitNumber n -> S.Float $ wrap $ show n
  LitString s -> S.StringExpr $ wrap s
  LitChar c -> codegenChar c
  LitBoolean b -> S.Bool b
  LitArray a -> C.vector $ codegenExpr codegenEnv <$> a
  LitRecord r -> C.record $ (map $ codegenExpr codegenEnv) <$> r

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
codegenEffectChain codegenEnv = C.thunk <<< codegenChain effectChainMode codegenEnv

codegenChain :: ChainMode -> CodegenEnv -> NeutralExpr -> ChezExpr
codegenChain chainMode codegenEnv = collect []
  where
  recursive :: Boolean
  recursive = false

  -- `expression` has type `Effect ..`, so we can confidently unthunk here
  codegenEffectBind :: NeutralExpr -> ChezExpr
  codegenEffectBind expression = case unwrap expression of
    PrimEffect e' ->
      codegenPrimEffect codegenEnv e'
    UncurriedEffectApp f p ->
      C.runUncurriedFn (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
    _ ->
      C.unthunk $ codegenExpr codegenEnv expression

  finish :: Boolean -> Array _ -> NeutralExpr -> ChezExpr
  finish shouldUnthunk bindings expression = do
    let
      maybeUnthunk :: ChezExpr -> ChezExpr
      maybeUnthunk = if shouldUnthunk then C.unthunk else identity
    case NEA.fromArray bindings of
      Nothing -> maybeUnthunk $ codegenExpr codegenEnv expression
      Just bindings' -> S.Let recursive bindings' $ maybeUnthunk $ codegenExpr codegenEnv expression

  collect :: Array _ -> NeutralExpr -> ChezExpr
  collect bindings expression = case unwrap expression of
    Let i l v e' ->
      collect (Array.snoc bindings $ Tuple (toChezIdent i l) (codegenExpr codegenEnv v)) e'
    EffectPure e' | chainMode.effect ->
      finish false bindings e'
    PrimEffect e' | chainMode.effect ->
      codegenPrimEffect codegenEnv e'
    EffectBind i l v e' | chainMode.effect ->
      collect
        (Array.snoc bindings $ Tuple (toChezIdent i l) (codegenEffectBind v))
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
        C.app
          (S.Identifier $ C.recordTypePredicate $ flattenQualified currentModule qi)
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

codegenPrimEffect :: CodegenEnv -> BackendEffect NeutralExpr -> ChezExpr
codegenPrimEffect codegenEnv = case _ of
  EffectRefNew v ->
    C.app (S.Identifier $ scmPrefixed "box") (codegenExpr codegenEnv v)
  EffectRefRead r ->
    C.app (S.Identifier $ scmPrefixed "unbox") (codegenExpr codegenEnv r)
  EffectRefWrite r v ->
    S.List
      [ S.Identifier $ scmPrefixed "set-box!", codegenExpr codegenEnv r, codegenExpr codegenEnv v ]
