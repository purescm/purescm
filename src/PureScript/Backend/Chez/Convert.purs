module PureScript.Backend.Chez.Convert where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..), uncurry)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Pair(..))
import Safe.Coerce (coerce)

type CodegenEnv =
  { currentModule :: ModuleName
  }

codegenModule :: BackendModule -> ChezLibrary
codegenModule { name, bindings, exports, imports, foreign: foreign_ } =
  let
    codegenEnv :: CodegenEnv
    codegenEnv = { currentModule: name }

    exports' :: Array ChezExport
    exports' = map ExportIdentifier $ coerce $ Array.fromFoldable exports

    definitions :: Array ChezDefinition
    definitions = Array.concat $
      codegenTopLevelBindingGroup codegenEnv <$> bindings

    pursImports :: Array ChezImport
    pursImports = Array.fromFoldable imports <#> \importedModule ->
      ImportSet $ ImportPrefix
        ( ImportLibrary
            { identifiers: NEA.cons' (coerce importedModule) [ "lib" ], version: Nothing }
        )
        (coerce importedModule <> ".")

    foreignImport :: Array ChezImport
    foreignImport
      | Set.isEmpty foreign_ = []
      | otherwise =
          [ ImportSet $
              ImportLibrary
                { identifiers: NEA.cons' (coerce name) [ "foreign" ], version: Nothing }
          ]
  in
    { "#!r6rs": true
    , "#!chezscheme": true
    , name:
        { identifiers: NEA.cons' (coerce name) [ "lib" ]
        , version: []
        }
    , imports:
        [ ImportSet $ ImportPrefix
            (ImportLibrary { identifiers: NEA.singleton "chezscheme", version: Nothing })
            "scm:"
        , ImportSet $ ImportPrefix
            (ImportLibrary { identifiers: NEA.cons' "_Chez_Runtime" [ "lib" ], version: Nothing })
            "rt:"
        ] <> pursImports <> foreignImport
    , exports: exports'
    , body: { definitions, expressions: [] }
    }

codegenTopLevelBindingGroup
  :: CodegenEnv -> BackendBindingGroup Ident NeutralExpr -> Array ChezDefinition
codegenTopLevelBindingGroup codegenEnv { recursive, bindings }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings =
      [ DefineValue "rtlbg" $ S.Identifier "recursive-top-level-binding-group" ]
  | otherwise =
      codegenTopLevelBinding codegenEnv <$> bindings

codegenTopLevelBinding :: CodegenEnv -> Tuple Ident NeutralExpr -> ChezDefinition
codegenTopLevelBinding codegenEnv (Tuple (Ident i) n) =
  case unwrap n of
    Abs a e ->
      DefineCurriedFunction i (uncurry S.toChezIdent <$> a) (codegenExpr codegenEnv e)
    UncurriedAbs a e ->
      DefineUncurriedFunction i (uncurry S.toChezIdent <$> a) (codegenExpr codegenEnv e)
    CtorDef _ _ _ ss ->
      case NonEmptyArray.fromArray ss of
        Nothing -> DefineUncurriedFunction i [] $ codegenExpr codegenEnv n
        Just xs -> DefineCurriedFunction i xs $ codegenExpr codegenEnv n
    _ ->
      DefineValue i $ codegenExpr codegenEnv n

codegenExpr :: CodegenEnv -> NeutralExpr -> ChezExpr
codegenExpr codegenEnv@{ currentModule } (NeutralExpr s) = case s of
  Var (Qualified (Just moduleName) (Ident v))
    | currentModule == moduleName -> S.Identifier v
    | otherwise -> S.Identifier $ coerce moduleName <> "." <> v
  Var (Qualified Nothing (Ident v)) ->
    S.Identifier v
  Local i l ->
    S.Identifier $ coerce $ S.toChezIdent i l
  Lit l ->
    codegenLiteral codegenEnv l
  App f p ->
    S.chezCurriedApplication (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    S.chezCurriedFunction (uncurry S.toChezIdent <$> a) (codegenExpr codegenEnv e)

  UncurriedApp _ _ ->
    S.Identifier "uncurried-app"
  UncurriedAbs _ _ ->
    S.Identifier "uncurried-abs"

  UncurriedEffectApp _ _ ->
    S.Identifier "uncurried-effect-app"
  UncurriedEffectAbs _ _ ->
    S.Identifier "uncurried-effect-ab"

  Accessor _ _ ->
    S.Identifier "object-accessor"
  Update _ _ ->
    S.Identifier "object-update"

  CtorSaturated _ _ _ _ _ ->
    S.Identifier "ctor-saturated"
  CtorDef _ _ i ss ->
    codegenCtorDef i ss

  LetRec _ _ _ ->
    S.Identifier "let-rec"
  Let i l v e ->
    S.chezLet (S.toChezIdent i l) (codegenExpr codegenEnv v) (codegenExpr codegenEnv e)
  Branch b o -> do
    let
      goPair :: Pair NeutralExpr -> { c :: _, e :: _ }
      goPair (Pair c e) = { c: codegenExpr codegenEnv c, e: codegenExpr codegenEnv e }
    S.chezCond (goPair <$> b) (codegenExpr codegenEnv <$> o)

  EffectBind _ _ _ _ ->
    S.Identifier "effect-bind"
  EffectPure _ ->
    S.Identifier "effect-pure"
  EffectDefer _ ->
    S.Identifier "effect-defer"

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
      [ S.Identifier "scm:raise"
      , S.List
          [ S.Identifier "scm:condition"
          , S.List [ S.Identifier "scm:make-error" ]
          , S.List
              [ S.Identifier "scm:make-message-condition"
              , S.String $ Json.stringify $ Json.fromString i
              ]
          ]
      ]

codegenLiteral :: CodegenEnv -> Literal NeutralExpr -> ChezExpr
codegenLiteral codegenEnv = case _ of
  LitInt i -> S.Integer $ wrap $ show i
  LitNumber n -> S.Float $ wrap $ show n
  LitString s -> S.String $ Json.stringify $ Json.fromString s
  LitChar c -> S.Identifier $ "#\\" <> CodeUnits.singleton c
  LitBoolean b -> S.Identifier $ if b then "#t" else "#f"
  LitArray a -> S.vector $ codegenExpr codegenEnv <$> a
  LitRecord r -> S.record $ (map $ codegenExpr codegenEnv) <$> r

codegenPrimOp :: CodegenEnv -> BackendOperator NeutralExpr -> ChezExpr
codegenPrimOp codegenEnv = case _ of
  Op1 o x -> do
    let
      x' = codegenExpr codegenEnv x
    case o of
      OpBooleanNot ->
        S.List [ S.Identifier "scm:not", x' ]
      OpIntBitNot ->
        S.List [ S.Identifier "scm:fxlognot", x' ]
      OpIntNegate ->
        S.List [ S.Identifier "scm:fx-", x' ]
      OpNumberNegate ->
        S.List [ S.Identifier "scm:fl-", x' ]
      OpArrayLength ->
        S.List [ S.Identifier "scm:vector-length", x' ]
      OpIsTag _ ->
        S.Identifier "op-is-tag"
  Op2 o x y ->
    let
      x' = codegenExpr codegenEnv x
      y' = codegenExpr codegenEnv y

      opFixNum = case _ of
        OpAdd -> S.Identifier "scm:fx+"
        OpSubtract -> S.Identifier "scm:fx-"
        OpMultiply -> S.Identifier "scm:fx*"
        OpDivide -> S.Identifier "scm:fx/"

      opFloNum = case _ of
        OpAdd -> S.Identifier "scm:fl+"
        OpSubtract -> S.Identifier "scm:fl-"
        OpMultiply -> S.Identifier "scm:fl*"
        OpDivide -> S.Identifier "scm:fl/"

      makeComparison :: String -> BackendOperatorOrd -> ChezExpr
      makeComparison tyName = do
        let
          comparisonExpression :: String -> ChezExpr
          comparisonExpression opName =
            S.List [ S.Identifier $ Array.fold [ "scm:", tyName, opName ], x', y' ]
        case _ of
          OpEq -> comparisonExpression "=?"
          OpNotEq -> S.List [ S.Identifier "scm:not", comparisonExpression "=?" ]
          OpGt -> comparisonExpression ">?"
          OpGte -> comparisonExpression ">=?"
          OpLt -> comparisonExpression "<?"
          OpLte -> comparisonExpression "<=?"
    in
      case o of
        OpArrayIndex ->
          S.List [ S.Identifier "scm:vector-ref", x', y' ]
        OpBooleanAnd ->
          S.List [ S.Identifier "scm:and", x', y' ]
        OpBooleanOr ->
          S.List [ S.Identifier "scm:or", x', y' ]
        OpBooleanOrd o' -> do
          -- Gt, Gte, Lt, Lte are defined under rt:
          let
            comparisonExpression :: String -> String -> ChezExpr
            comparisonExpression prefix opName =
              S.List [ S.Identifier $ Array.fold [ prefix, "boolean", opName ], x', y' ]
          case o' of
            OpEq -> comparisonExpression "scm:" "=?"
            OpNotEq -> S.List [ S.Identifier "scm:not", comparisonExpression "scm:" "=?" ]
            OpGt -> comparisonExpression "rt:" ">?"
            OpGte -> comparisonExpression "rt:" ">=?"
            OpLt -> comparisonExpression "rt:" "<?"
            OpLte -> comparisonExpression "rt:" "<=?"
        OpCharOrd o' ->
          makeComparison "char" o'
        OpIntBitAnd ->
          S.List [ S.Identifier "scm:fxlogand", x', y' ]
        OpIntBitOr ->
          S.List [ S.Identifier "scm:fxlogor", x', y' ]
        OpIntBitShiftLeft ->
          -- shift-left-logical = shift-left-arithmetic
          S.List [ S.Identifier "scm:fxsll", x', y' ]
        OpIntBitShiftRight ->
          -- shift-right-arithmetic preserves signs
          S.List [ S.Identifier "scm:fxsra", x', y' ]
        OpIntBitXor ->
          S.List [ S.Identifier "scm:fxlogxor", x', y' ]
        OpIntBitZeroFillShiftRight ->
          -- shift-right-logical follows the invariant:
          --
          -- (= (most-positive-fixnum) (fxsrl -1 1))
          --
          -- or:
          --
          -- top == zshr (-1) 1
          S.List [ S.Identifier "scm:fxsrl", x', y' ]
        OpIntNum o' ->
          S.List [ opFixNum o', x', y' ]
        OpIntOrd o' ->
          makeComparison "fx" o'
        OpNumberNum o' ->
          S.List [ opFloNum o', x', y' ]
        OpNumberOrd o' ->
          makeComparison "fl" o'
        OpStringAppend ->
          S.List [ S.Identifier "scm:string-append", x', y' ]
        OpStringOrd o' ->
          makeComparison "string" o'

codegenCtorDef :: Ident -> Array String -> ChezExpr
codegenCtorDef (Ident i) [] =
  S.List
    [ S.Identifier "scm:cons"
    , S.List [ S.Identifier "scm:quote", S.Identifier i ]
    , S.List [ S.Identifier "scm:quote", S.Identifier "()" ]
    ]
codegenCtorDef (Ident i) ss =
  S.List
    [ S.Identifier "scm:cons"
    , S.List [ S.Identifier "scm:quote", S.Identifier i ]
    , S.List $ [ S.Identifier "scm:vector" ] <> map S.Identifier ss
    ]
