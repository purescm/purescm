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
import Data.Tuple as Tuple
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Chez.Constants (libChezSchemePrefix, moduleForeign, moduleLib, runtimePrefix, rtPrefixed, scmPrefixed)
import PureScript.Backend.Chez.Syntax (ChezDefinition(..), ChezExport(..), ChezExpr, ChezImport(..), ChezImportSet(..), ChezLibrary)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Pair(..))
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
      $ Array.concatMap S.definitionIdentifiers definitions
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
          (uncurry S.toChezIdent <$> a)
          (codegenExpr codegenEnv e)
      ]
    UncurriedAbs a e ->
      [ DefineUncurriedFunction
          i
          (uncurry S.toChezIdent <$> a)
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
                  $ S.chezUncurriedApplication
                      (S.Identifier $ S.recordTypeUncurriedConstructor i)
                      (map S.Identifier ss)
              ]
    _ ->
      [ DefineValue i $ codegenExpr codegenEnv n ]

codegenExpr :: CodegenEnv -> NeutralExpr -> ChezExpr
codegenExpr codegenEnv@{ currentModule } s = case unwrap s of
  Var qi ->
    S.Identifier $ S.resolve currentModule qi
  Local i l ->
    S.Identifier $ coerce $ S.toChezIdent i l
  Lit l ->
    codegenLiteral codegenEnv l
  App f p ->
    S.chezCurriedApplication (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    S.chezCurriedFunction (uncurry S.toChezIdent <$> a) (codegenExpr codegenEnv e)

  UncurriedApp fn args ->
    S.List $ Array.cons (codegenExpr codegenEnv fn) $
      (codegenExpr codegenEnv <$> args)
  UncurriedAbs args body ->
    S.List
      [ S.Identifier $ scmPrefixed "lambda"
      , S.List $ (S.Identifier <<< uncurry S.toChezIdent) <$> args
      , S.List [ codegenExpr codegenEnv body ]
      ]

  UncurriedEffectApp _ _ ->
    S.Identifier "uncurried-effect-app"
  UncurriedEffectAbs _ _ ->
    S.Identifier "uncurried-effect-ab"

  Accessor e (GetProp i) ->
    S.chezUncurriedApplication
      (S.Identifier $ rtPrefixed "hashtable-get")
      [ codegenExpr codegenEnv e
      , S.String $ Json.stringify $ Json.fromString i
      ]
  Accessor e (GetIndex i) ->
    S.chezUncurriedApplication
      (S.Identifier $ scmPrefixed "vector-ref")
      [ codegenExpr codegenEnv e, S.Integer $ wrap $ show i ]
  Accessor e (GetCtorField qi _ _ _ field _) ->
    S.recordAccessor (codegenExpr codegenEnv e) (S.resolve currentModule qi) field
  Update _ _ ->
    S.Identifier "object-update"

  CtorSaturated qi _ _ _ xs ->
    S.chezUncurriedApplication
      (S.Identifier $ S.recordTypeUncurriedConstructor $ S.resolve currentModule qi)
      (map (codegenExpr codegenEnv <<< Tuple.snd) xs)
  CtorDef _ _ _ _ ->
    unsafeCrashWith "codegenExpr:CtorDef - handled by codegenTopLevelBinding!"

  LetRec _ _ _ ->
    S.Identifier "let-rec"
  Let _ _ _ _ ->
    codegenPureChain codegenEnv s
  Branch b o -> do
    let
      goPair :: Pair NeutralExpr -> { c :: _, e :: _ }
      goPair (Pair c e) = { c: codegenExpr codegenEnv c, e: codegenExpr codegenEnv e }
    S.chezCond (goPair <$> b) (codegenExpr codegenEnv <$> o)

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
              , S.String $ Json.stringify $ Json.fromString i
              ]
          ]
      ]

codegenLiteral :: CodegenEnv -> Literal NeutralExpr -> ChezExpr
codegenLiteral codegenEnv = case _ of
  LitInt i -> S.Integer $ wrap $ show i
  LitNumber n -> S.Float $ wrap $ show n
  LitString s -> S.String $ S.jsonToChezString $ Json.stringify $ Json.fromString s
  LitChar c -> S.Identifier $ "#\\" <> CodeUnits.singleton c
  LitBoolean b -> S.Identifier $ if b then "#t" else "#f"
  LitArray a -> S.vector $ codegenExpr codegenEnv <$> a
  LitRecord r -> S.record $ (map $ codegenExpr codegenEnv) <$> r

type BlockMode = { effect :: Boolean }

pureBlockMode :: BlockMode
pureBlockMode = { effect: false }

effectBlockMode :: BlockMode
effectBlockMode = { effect: true }

codegenPureChain :: CodegenEnv -> NeutralExpr -> ChezExpr
codegenPureChain codegenEnv = codegenChain pureBlockMode codegenEnv

codegenEffectChain :: CodegenEnv -> NeutralExpr -> ChezExpr
codegenEffectChain codegenEnv = S.chezThunk <<< codegenChain effectBlockMode codegenEnv

codegenChain :: BlockMode -> CodegenEnv -> NeutralExpr -> ChezExpr
codegenChain blockMode codegenEnv = go []
  where
  go :: Array _ -> NeutralExpr -> ChezExpr
  go bindings expression = case unwrap expression of
    Let i l v e' ->
      go (Array.cons { i, l, v: codegenExpr codegenEnv v } bindings) e'
    EffectBind i l v e' | blockMode.effect ->
      go (Array.cons { i, l, v: S.chezUnthunk $ codegenExpr codegenEnv v } bindings) e'
    EffectPure e' | blockMode.effect ->
      go bindings e'
    EffectDefer e' | blockMode.effect ->
      go bindings e'
    _ | Array.null bindings ->
      codegenExpr codegenEnv expression
    _ ->
      S.List $
        [ S.Identifier $ scmPrefixed "let*"
        , S.List $ Array.reverse bindings <#> \binding ->
            S.List [ S.Identifier $ S.toChezIdent binding.i binding.l, binding.v ]
        , codegenExpr codegenEnv expression
        ]

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
          (S.Identifier $ S.recordTypePredicate $ S.resolve currentModule qi)
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
