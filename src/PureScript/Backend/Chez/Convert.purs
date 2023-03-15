module PureScript.Backend.Chez.Convert where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Profunctor.Strong (second)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple, uncurry)
import PureScript.Backend.Chez.Syntax (ChezExpr)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))
import Safe.Coerce (coerce)

type CodegenEnv =
  { currentModule :: ModuleName
  }

codegenModule :: BackendModule -> ChezExpr
codegenModule { name, bindings, exports, imports } =
  let
    codegenEnv :: CodegenEnv
    codegenEnv = { currentModule: name }

    exports' :: Array Ident
    exports' = Array.fromFoldable exports

    imports' :: Array ModuleName
    imports' = Array.fromFoldable imports

    bindings' :: Array ChezExpr
    bindings' = Array.concat $ codegenTopLevelBindingGroup codegenEnv <$> bindings
  in
    S.library name exports' imports' bindings'

codegenTopLevelBindingGroup :: CodegenEnv -> BackendBindingGroup Ident NeutralExpr -> Array ChezExpr
codegenTopLevelBindingGroup codegenEnv { recursive, bindings }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings =
      [ S.Identifier "recursive-top-level-binding-group" ]
  | otherwise = codegenBindings codegenEnv bindings

codegenBindings :: CodegenEnv -> Array (Tuple Ident NeutralExpr) -> Array ChezExpr
codegenBindings codegenEnv = map (coerce $ uncurry S.define) <<< map
  (second $ codegenExpr codegenEnv)

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
  Branch _ _ ->
    S.Identifier "branch"

  EffectBind _ _ _ _ ->
    S.Identifier "effect-bind"
  EffectPure _ ->
    S.Identifier "effect-pure"
  EffectDefer _ ->
    S.Identifier "effect-defer"

  PrimOp _ ->
    S.Identifier "prim-op"
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
