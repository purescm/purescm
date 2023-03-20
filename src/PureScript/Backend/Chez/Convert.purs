module PureScript.Backend.Chez.Convert where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype (un, unwrap, wrap)
import Data.Set as Set
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..), uncurry)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))
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
        (coerce importedModule)

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
        ] <> pursImports <> foreignImport
    , exports: exports'
    , body: { definitions, exprs: [] }
    }

codegenTopLevelBindingGroup
  :: CodegenEnv -> BackendBindingGroup Ident NeutralExpr -> Array ChezDefinition
codegenTopLevelBindingGroup codegenEnv { recursive, bindings }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings =
      [ DefineValue "rtlbg" $ S.Identifier "recursive-top-level-binding-group" ]
  | otherwise =
      map (codegenTopLevelBinding codegenEnv) bindings

codegenTopLevelBinding :: CodegenEnv -> Tuple Ident NeutralExpr -> ChezDefinition
codegenTopLevelBinding codegenEnv@{ currentModule } (Tuple (Ident ident) expr) = case unwrap expr of
  Var (Qualified (Just moduleName) (Ident v))
    | currentModule == moduleName -> DefineValue ident $ S.Identifier v
    | otherwise -> DefineValue ident $ S.Identifier $ coerce moduleName <> "." <> v
  Var (Qualified Nothing (Ident v)) ->
    DefineValue ident $ S.Identifier v
  Local i l ->
    DefineValue ident $ S.Identifier $ coerce $ S.toChezIdent i l
  Lit l ->
    DefineValue ident $ codegenLiteral codegenEnv l
  App f p ->
    DefineValue ident $ S.chezCurriedApplication (codegenExpr codegenEnv f)
      (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    DefineCurriedFunction
      ident
      (map (un Ident <<< uncurry S.toChezIdent) $ a)
      (codegenExpr codegenEnv e)

  UncurriedApp _ _ ->
    DefineValue ident $ S.Identifier "uncurried-app"
  UncurriedAbs _ _ ->
    DefineValue ident $ S.Identifier "uncurried-abs"

  UncurriedEffectApp _ _ ->
    DefineValue ident $ S.Identifier "uncurried-effect-app"
  UncurriedEffectAbs _ _ ->
    DefineValue ident $ S.Identifier "uncurried-effect-ab"

  Accessor _ _ ->
    DefineValue ident $ S.Identifier "object-accessor"
  Update _ _ ->
    DefineValue ident $ S.Identifier "object-update"

  CtorSaturated _ _ _ _ _ ->
    DefineValue ident $ S.Identifier "ctor-saturated"
  CtorDef _ _ _ _ ->
    DefineValue ident $ S.Identifier "ctor-def"

  LetRec _ _ _ ->
    DefineValue ident $ S.Identifier "let-rec"
  Let i l v e ->
    DefineValue ident $ S.chezLet (S.toChezIdent i l) (codegenExpr codegenEnv v)
      (codegenExpr codegenEnv e)
  Branch _ _ ->
    DefineValue ident $ S.Identifier "branch"

  EffectBind _ _ _ _ ->
    DefineValue ident $ S.Identifier "effect-bind"
  EffectPure _ ->
    DefineValue ident $ S.Identifier "effect-pure"
  EffectDefer _ ->
    DefineValue ident $ S.Identifier "effect-defer"

  PrimOp _ ->
    DefineValue ident $ S.Identifier "prim-op"
  PrimEffect _ ->
    DefineValue ident $ S.Identifier "prim-effect"
  PrimUndefined ->
    DefineValue ident $ S.Identifier "prim-undefined"

  Fail _ ->
    DefineValue ident $ S.Identifier "fail"

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
