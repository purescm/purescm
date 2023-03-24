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
  CtorDef _ _ _ _ ->
    S.Identifier "ctor-def"

  LetRec _ _ _ ->
    S.Identifier "let-rec"
  Let i l v e ->
    S.chezLet (S.toChezIdent i l) (codegenExpr codegenEnv v) (codegenExpr codegenEnv e)
  Branch b o ->
    let
      goPair :: Pair NeutralExpr -> { c :: _, e :: _ }
      goPair (Pair c e) = { c: codegenExpr codegenEnv c, e: codegenExpr codegenEnv e }
    in
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

  Fail _ ->
    S.Identifier "fail"

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
  Op1 o x ->
    let
      x' = codegenExpr codegenEnv x
    in
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

      opBooleanOrd = case _ of
        OpEq -> S.Identifier "scm:boolean=?"
        OpNotEq -> S.Identifier "scm:boolean=?"
        OpGt -> S.Identifier "rt:boolean<?"
        OpGte -> S.Identifier "rt:boolean>=?"
        OpLt -> S.Identifier "rt:boolean<?"
        OpLte -> S.Identifier "rt:boolean<=?"

      opFixNum = case _ of
        OpAdd -> S.Identifier "scm:fx+"
        OpSubtract -> S.Identifier "scm:fx-"
        OpMultiply -> S.Identifier "scm:fx*"
        OpDivide -> S.Identifier "scm:fx/"

      opFixOrd = case _ of
        OpEq -> S.Identifier "scm:fx="
        OpNotEq -> S.Identifier "scm:fx="
        OpGt -> S.Identifier "scm:fx>"
        OpGte -> S.Identifier "scm:fx>="
        OpLt -> S.Identifier "scm:fx<"
        OpLte -> S.Identifier "scm:fx<="

      opFloNum = case _ of
        OpAdd -> S.Identifier "scm:fl+"
        OpSubtract -> S.Identifier "scm:fl-"
        OpMultiply -> S.Identifier "scm:fl*"
        OpDivide -> S.Identifier "scm:fl/"

      opFloOrd = case _ of
        OpEq -> S.Identifier "scm:fl="
        OpNotEq -> S.Identifier "scm:fl="
        OpGt -> S.Identifier "scm:fl>"
        OpGte -> S.Identifier "scm:fl>="
        OpLt -> S.Identifier "scm:fl<"
        OpLte -> S.Identifier "scm:fl<="

      opCharOrd = case _ of
        OpEq -> S.Identifier "scm:char=?"
        OpNotEq -> S.Identifier "scm:char=?"
        OpGt -> S.Identifier "scm:char>?"
        OpGte -> S.Identifier "scm:char>=?"
        OpLt -> S.Identifier "scm:char<?"
        OpLte -> S.Identifier "scm:char<=?"

      opStringOrd = case _ of
        OpEq -> S.Identifier "scm:string=?"
        OpNotEq -> S.Identifier "scm:string=?"
        OpGt -> S.Identifier "scm:string>?"
        OpGte -> S.Identifier "scm:string>=?"
        OpLt -> S.Identifier "scm:string<?"
        OpLte -> S.Identifier "scm:string<=?"
    in
      case o of
        OpArrayIndex ->
          S.List [ S.Identifier "scm:vector-ref", x', y' ]
        OpBooleanAnd ->
          S.List [ S.Identifier "scm:and", x', y' ]
        OpBooleanOr ->
          S.List [ S.Identifier "scm:or", x', y' ]
        OpBooleanOrd o' ->
          case o' of
            OpNotEq ->
              S.List [ S.Identifier "scm:not", S.List [ opBooleanOrd o', x', y' ] ]
            _ ->
              S.List [ opBooleanOrd o', x', y' ]
        OpCharOrd o' ->
          case o' of
            OpNotEq ->
              S.List [ S.Identifier "scm:not", S.List [ opCharOrd o', x', y' ] ]
            _ ->
              S.List [ opCharOrd o', x', y' ]
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
          case o' of
            OpNotEq ->
              S.List [ S.Identifier "scm:not", S.List [ opFixOrd o', x', y' ] ]
            _ ->
              S.List [ opFixOrd o', x', y' ]
        OpNumberNum o' ->
          S.List [ opFloNum o', x', y' ]
        OpNumberOrd o' ->
          case o' of
            OpNotEq ->
              S.List [ S.Identifier "scm:not", S.List [ opFloOrd o', x', y' ] ]
            _ ->
              S.List [ opFloOrd o', x', y' ]
        OpStringAppend ->
          S.List [ S.Identifier "scm:string-append", x', y' ]
        OpStringOrd o' ->
          -- We've done this pattern-matching four times. Should this
          -- be refactored to have the pretty-printer deal with it?
          case o' of
            OpNotEq ->
              S.List [ S.Identifier "scm:not", S.List [ opStringOrd o', x', y' ] ]
            _ ->
              S.List [ opStringOrd o', x', y' ]
