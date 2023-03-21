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
import PureScript.Backend.Optimizer.Syntax (BackendOperator(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Pair(..))
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
  Op1 _ _ ->
    S.Identifier "unary-prim-op"
  Op2 o x y ->
    let
      x' = codegenExpr codegenEnv x
      y' = codegenExpr codegenEnv y

      opDtNum = case _ of
        OpAdd -> S.Identifier "scm:+"
        OpSubtract -> S.Identifier "scm:-"
        OpMultiply -> S.Identifier "scm:*"
        OpDivide -> S.Identifier "scm:/"

      opDtOrd = case _ of
        OpEq -> S.Identifier "scm:="
        OpNotEq -> S.Identifier "scm:="
        OpGt -> S.Identifier "scm:>"
        OpGte -> S.Identifier "scm:>="
        OpLt -> S.Identifier "scm:<"
        OpLte -> S.Identifier "scm:<="
    in
      case o of
        OpIntNum o' ->
          S.List [ opDtNum o', x', y' ]
        OpIntOrd o' ->
          case o' of
            OpNotEq ->
              S.List [ S.Identifier "scm:not", S.List [ opDtOrd o', x', y' ] ]
            _ ->
              S.List [ opDtOrd o', x', y' ]
        OpNumberNum o' ->
          S.List [ opDtNum o', x', y' ]
        OpNumberOrd o' ->
          case o' of
            OpNotEq ->
              S.List [ S.Identifier "scm:not", S.List [ opDtOrd o', x', y' ] ]
            _ ->
              S.List [ opDtOrd o', x', y' ]
        _ ->
          S.Identifier "binary-prim-op"
