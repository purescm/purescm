module Language.PureScript.Scheme.CodeGen.Transpiler where

import Data.Text (Text)

import Language.PureScript.Names
       (Qualified(..), runIdent, runProperName, showQualified, disqualify)
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr
       (Expr(..), Bind(..), Guard, CaseAlternative(..))
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.AST.Literals (Literal(..))

import Language.PureScript.Scheme.Util (mapWithIndex, concatMapWithIndex)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..), everywhere)
import Language.PureScript.Scheme.CodeGen.Scheme
       (t, define, lambda1, eq2, eqQ, stringEqQ2, and_, quote, cons, car, cdr,
        cond', vector, vectorRef)


-- TODO: translate a PureScript module to a Scheme library instead.
moduleToScheme :: Module Ann -> [SExpr]
moduleToScheme (Module _sourceSpan _comments moduleName _path
                       _imports _exports _reExports _foreigns declarations) =
  concatMap topLevelBindToScheme declarations

  where

    ----------------------------------------------------------------------------

    topLevelBindToScheme :: Bind Ann -> [SExpr]

    topLevelBindToScheme (NonRec _ann ident expr) =
      [define (runIdent ident) (exprToScheme expr)]

    topLevelBindToScheme (Rec xs) =
      [ define (runIdent ident) (exprToScheme expr)
      | ((_ann, ident), expr) <- xs ]

    ----------------------------------------------------------------------------

    exprToScheme :: Expr Ann -> SExpr

    exprToScheme (Literal _ann literal) = literalToScheme literal

    -- An ADT is translated to a tagged pair. The car of the pair is the tag,
    -- holding the name of the Constructor as a quoted symbol. The cdr of the
    -- pair is a vector, holding the values of the Constructor, if any.
    --
    -- A Constructor is either a pair literal in the case of a simple sum type
    -- (e.g. data Foo = Bar | Baz) or a a function that produces a tagged pair
    -- holding the values for the constructed ADT.
    --
    -- A product type with N fields results into a curried function of N arguments.
    --     data Foo = Bar Int Int
    --     (lambda (v0) (lambda (v1) (cons 'Bar (vector v0 v1)))) ; defined as Bar
    --
    -- A sum type results into multiple Constructor. If the sum type is `simple'
    -- we just emit a constant representing that constructed value.
    --     data Foo = Bar | Baz
    --     (cons (quote Bar) (vector)) ; defined as Bar
    --     (cons (quote Baz) (vector)) ; defined as Baz
    -- TODO: maybe we could remove that empty vector?
    --
    -- If the sum type is not simple, so its constructors could be product types
    -- we emit multiple functions.
    --     data Foo = Bar Int Int
    --              | Baz
    --     (lambda (v0) (lambda (v1)) (cons 'Bar (vector v0 v1))) ; defined as Bar
    --     (cons (quote Baz) (vector))                            ; defined as Baz
    exprToScheme (Constructor _ann _typeName constructorName fields) =
      go fields
      where
        go (x:xs) = lambda1 (runIdent x) (go xs)
        go [] = cons (quote (Symbol (runProperName constructorName)))
                     (vector (fmap (\field -> Symbol (runIdent field)) fields))

    exprToScheme (Var _ann (Qualified Nothing ident)) = Symbol (runIdent ident)
    exprToScheme (Var _ann q@(Qualified (Just moduleName') ident))
      | moduleName == moduleName' = Symbol (runIdent ident)
      | otherwise = Symbol (showQualified runIdent q)
      
    -- `values' holds the values we're matching against, for instance it could be
    -- a list of Vars where each Var is the LHS of the pattern matching. For instance
    -- in `foo 1 a = 0', `1' and `a' are values.
    -- `caseAlternatives' holds the various cases we're matching against such values.
    -- for each case there is a binder and a possible result if the match holds.
    exprToScheme (Case _ann values caseAlternatives) =
      caseToScheme values caseAlternatives

    exprToScheme (Abs _ann arg expr) =
      lambda1 (runIdent arg) (exprToScheme expr)

    exprToScheme (App _ann function arg) =
      List [exprToScheme function, exprToScheme arg]

    exprToScheme _ = error "Not implemented"

    ----------------------------------------------------------------------------

    literalToScheme :: Literal (Expr Ann) -> SExpr
    literalToScheme (NumericLiteral (Left integer)) = Integer integer
    literalToScheme (StringLiteral x) = String x
    literalToScheme (ArrayLiteral xs) = vector $ fmap exprToScheme xs
    literalToScheme _ = error "Not implemented"

    ----------------------------------------------------------------------------

    caseToScheme :: [Expr Ann] -> [CaseAlternative Ann] -> SExpr
    caseToScheme values caseAlternatives =
      cond' clauses
      where
        clauses = concatMap condClauses caseExpr
        caseExpr = straightenCaseExpr values caseAlternatives

        -- Emit clauses for a cond expression.
        -- In scheme: (cond clause1 clause2 ...)
        -- Return a list of pairs where the first element is the condition and
        -- the second element is the result.
        condClauses
          :: ([(Expr Ann, Binder Ann)], Either [(Expr Ann, Expr Ann)] (Expr Ann))
          -> [(SExpr, SExpr)]

        -- If there are guards we're provided with multiple results and each
        -- result is associated to a guard. Thus we have to emit multiple cond
        -- clauses for each result.
        condClauses (valuesAndBinders, Left guardsAndResults) =
          map (\(guard, result) ->
                (replaceVariables (exprToScheme guard)
                                  (variablesToReplace valuesAndBinders),
                replaceVariables (exprToScheme result)
                                 (variablesToReplace valuesAndBinders)))
              guardsAndResults

        -- If there are no guards we're provided with a single result. In this
        -- case we can emit a single cond clause for the single result associated
        -- to multiple values and binders.
        condClauses (valuesAndBinders, Right result) =
          [(test valuesAndBinders, bindersToResult valuesAndBinders result)]

        -- Emit the test for a cond clause.
        -- In scheme: (cond ((and test1a test1b) result1) ...)
        -- 
        -- Always wrap the test in an `and' form since binderToTest has to return
        -- a list because when applied to a ConstructorBinder it returns a list
        -- of SExpr (while the other cases return a list of SExpr with a single
        -- element only).
        --
        -- We could have produced a single SExpr node instead of a list of SExpr
        -- in binderToTest (so that for a ConstructorBinder it would have emitted
        -- an `and' form and for all other cases a simple SExpr node) but we
        -- would have ended up with nested `and's: one coming from `test' and
        -- one coming from `binderToTest' when it handles a ConstructorBinder.
        --
        -- This means that we could end up with and `and' form with a single node
        -- (e.g. (and foo)) but this is very simple to optimize compared to
        -- nested `and' forms that the alternative would have caused.
        --
        -- Keep in mind that this means that we could end up in the case of a
        -- ConstructorBinder with something like:
        --     (cond ((and (eq? (car v) 'Foo) cond1)  result1)
        --           ((and (eq? (car v) 'Foo) cond2)) result2)
        -- TODO: Maybe we should avoid the repeating of the first check.
        test valueAndBinders =
          and_ (concatMap (\(value, binder) -> binderToTest (exprToScheme value)
                                                            binder)
                          valueAndBinders)

        -- Emit a cond clause test for each value and binder.
        -- E.g.: (value: 23, binder: 42) -> (= 23 42)
        binderToTest :: SExpr -> Binder Ann -> [SExpr]
        binderToTest _value (NullBinder _ann) = [t]
        binderToTest value (LiteralBinder _ann (NumericLiteral (Left integer))) =
          [eq2 value (Integer integer)]
        binderToTest value (LiteralBinder _ann (StringLiteral x)) =
          [stringEqQ2 value (String x)]
        binderToTest _value (LiteralBinder _ann _) = error "Not implemented"
        binderToTest _value (VarBinder _ann _ident) = [t]

        -- For a ConstructorBinder we have to emit at least one test, but more likely
        -- multiple tests.
        --
        -- The first test has to check if, given the tagged pair
        -- ('ConstructorName . fields), its head containing the Constructor name
        -- matches the `value' we're currently analyzing in the code.
        -- For instance, in:
        --     match (Foo x) = x
        -- The first test translates into
        --     (eq? (car value) (quote Foo))
        -- Where value is the expression holding (Foo x)
        -- 
        -- Then we have to check the constructor fields, if any. The
        -- ConstructorBinder could contain multiple binders, each one binding an
        -- identifier to an element of the Constructor. We have to compare and
        -- access those through vector-ref.
        -- For instance, in:
        --     match (Foo 1) = 1
        -- The remaining tests translate into:
        --     (= (vector-ref (cdr v) 0) 1)
        binderToTest value (ConstructorBinder _ann _typeName
                                              constructorName binders) =
          (:) (eqQ (car value)
                   (quote (Symbol (runProperName (disqualify constructorName)))))
              (concatMapWithIndex
                (\i b -> (binderToTest (vectorRef (cdr value) (Integer i))
                                       b))
                binders)

        -- Consider the case:
        --     data Foo = Bar Int
        --     match bar@(Bar i) = bar
        -- It get translated to:
        --    (define Bar (lambda (x) ('Bar . x)))
        --    (define match (lambda (v) (cond (eq? (car v) 'Bar) v)))
        -- `bar' (in PureScript) is an alias to `v' (in Scheme). `value' is an
        -- `Expr.Var "v"', and `ident' is an `Ident.Ident "bar"'.
        -- So in order to generate the test for the cond in the case of a
        -- NamedBinder we have to process its binder (with binderToTest) using
        -- the actual variable name `value' and not with the alias `ìdent'.
        -- We will have to use ident when we have to deal with the result associated
        -- to this case though.
        binderToTest value (NamedBinder _ann _ident binder) = binderToTest value binder

        -- Emit a cond clause result for multiple values and binders associated to
        -- a single result. This is done by replacing the identifiers in the result
        -- expression according to its VarBinders.
        bindersToResult :: [(Expr Ann, Binder a)] -> Expr Ann -> SExpr
        bindersToResult valuesAndBinders result =
          replaceVariables (exprToScheme result)
                           (variablesToReplace valuesAndBinders)


        variablesToReplace :: [(Expr Ann, Binder a)] -> [(Text, SExpr)]
        variablesToReplace valuesAndBinders =
          concatMap (\(value, binder) -> go (exprToScheme value) binder)
                    valuesAndBinders
          where
            go _value (NullBinder _ann) = []
            go _value (LiteralBinder _ann _literal) = []

            -- Each identifier in a VarBinder will be replaced by its
            -- corresponding value.
            -- E.g. `foo m n = m + n' will emit a function whose parameters are
            -- `v' and `v0', but the `m + n' expression will use `m' and `n' as
            -- variable.
            -- We have to replace each occurrence of `m' with `v' and each
            -- occurrence of `n' with `v0'.
            go value (VarBinder _ann ident) = [(runIdent ident, value)]

            -- For a ConstructorBinder we have to replace each identifier in its
            -- binders with an access to the right index of the vector in the cdr
            -- of the tagged pair holding the Constructor values.
            go value (ConstructorBinder _ann _typeName _constructorName binders) =
              mapWithIndex (\i b -> (go' b,
                                     vectorRef (cdr value) (Integer i)))
                           binders

            -- Consider the case:
            --     data Foo = Bar Int
            --     match bar@(Bar i) = bar
            -- It get translated to:
            --    (define Bar (lambda (x) ('Bar . x)))
            --    (define match (lambda (v) (cond (eq? (car v) 'Bar) v)))
            -- `value' is an `Expr.Var "v"', and `ident' is an
            -- `Ident.Ident "bar"'.
            -- We have to do multiple replacements here. First, we have to
            -- replace each each `ident' with `value'. Then, we have to apply
            -- other possible replacements for the binder associated to the
            -- NamedBinder.
            go value (NamedBinder _ann ident binder) =
              (:) (runIdent ident, value)
                  (go value binder)

            -- TODO: possible fire hazard. So far I've met no ConstructorBinder
            -- whose binders are anything but VarBinders. But if that doesn't
            -- hold then it will explode here.
            go' (VarBinder _ann ident) = runIdent ident
            go' _ = error "Not implemented"


-- From:
-- [Value1, Value2, Value3]
-- [CaseAlternativeA{binders=[BinderA1, BinderA2, BinderA3],
--                   result=ResA},
--  CaseAlternativeB{binders=[BinderB1, BinderB2, BinderB3],
--                  result=ResB}]
--
-- To:
-- [([(Value1, BinderA1), (Value2, BinderA2), (Value3, BinderA3)], ResA),
--  ([(Value1, BinderB1), (Value2, BinderB2), (Value3, BinderB3)], ResB)]
straightenCaseExpr
  :: [Expr Ann]
  -> [CaseAlternative Ann]
  -> [([(Expr Ann, Binder Ann)], Either [(Guard Ann, Expr Ann)] (Expr Ann))]
  --     ^Value^^  ^Binder^^^    ^Res^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
straightenCaseExpr values caseAlternatives =
  map (\(CaseAlternative binders result) -> (zip values binders, result))
      caseAlternatives


-- Replace each (Symbol from) into a `to' SExpr everywhere in ast
replaceVariables :: SExpr -> [(Text, SExpr)] -> SExpr
replaceVariables ast mapping = everywhere (go mapping) ast
  where
    go ((from, to):xs) (Symbol i) =
      if i == from then to else go xs (Symbol i)
    go [] ast' = ast'
    go _xs ast' = ast'
