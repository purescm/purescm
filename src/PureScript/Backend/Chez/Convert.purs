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
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Chez.Syntax (ChezExpr)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))
import Safe.Coerce (coerce)

codegenModule :: BackendModule -> ChezExpr
codegenModule { name, bindings, exports } =
  let
    exports' :: Array Ident
    exports' = Array.fromFoldable exports

    bindings' :: Array ChezExpr
    bindings' = Array.concat $ codegenTopLevelBindingGroup <$> bindings
  in
    S.library name exports' bindings'

codegenTopLevelBindingGroup :: BackendBindingGroup Ident NeutralExpr -> Array ChezExpr
codegenTopLevelBindingGroup { recursive, bindings }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings = unsafeCrashWith "undefined"
  | otherwise = codegenBindings bindings

codegenBindings :: Array (Tuple Ident NeutralExpr) -> Array ChezExpr
codegenBindings = map (coerce $ uncurry S.define) <<< map (second codegenExpr)

codegenExpr :: NeutralExpr -> ChezExpr
codegenExpr (NeutralExpr s) = case s of
  Var (Qualified (Just (ModuleName mn)) (Ident v)) ->
    S.Identifier $ mn <> "." <> v
  Var (Qualified _ (Ident v)) ->
    S.Identifier v
  Local _ _ ->
    S.Identifier "local-variable"

  Lit l ->
    codegenLiteral l

  App f p ->
    NonEmptyArray.foldl1 S.app $ NonEmptyArray.cons (codegenExpr f) (codegenExpr <$> p)
  Abs _ _ ->
    S.Identifier "abs"

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
  Let _ _ _ _ ->
    S.Identifier "let"
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

codegenLiteral :: Literal NeutralExpr -> ChezExpr
codegenLiteral = case _ of
  LitInt i -> S.Integer $ wrap $ show i
  LitNumber n -> S.Float $ wrap $ show n
  LitString s -> S.String $ Json.stringify $ Json.fromString s
  LitChar c -> S.Identifier $ "#\\" <> CodeUnits.singleton c
  LitBoolean b -> S.Identifier $ if b then "#t" else "#f"
  LitArray a -> S.vector $ codegenExpr <$> a
  LitRecord r -> S.record $ (map codegenExpr) <$> r
