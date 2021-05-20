module Language.PureScript.Scheme.CodeGen.Transpiler where

import Data.Text                              (Text)
import Language.PureScript.CoreFn.Module      (Module(..))
import Language.PureScript.CoreFn.Ann         (Ann)
import Language.PureScript.CoreFn.Expr        (Expr(..), Bind(..), Guard,
                                               CaseAlternative(..))
import Language.PureScript.CoreFn.Binders     (Binder(..))
import Language.PureScript.Names              (runIdent, showQualified)
import Language.PureScript.AST.Literals       (Literal(..))
import Language.PureScript.Scheme.CodeGen.AST (AST(..), everywhere)

-- TODO: translate a PureScript module to a Scheme library instead.
moduleToScheme :: Module Ann -> [AST]
moduleToScheme (Module _sourceSpan _comments _name _path
                       _imports _exports _reExports _foreigns declarations) =
  concatMap topLevelBindToScheme declarations


topLevelBindToScheme :: Bind Ann -> [AST]
topLevelBindToScheme (NonRec _ann ident expr) =
  [Define (runIdent ident) (exprToScheme expr)]
topLevelBindToScheme (Rec xs) =
  [ Define (runIdent ident) (exprToScheme expr)
  | ((_ann, ident), expr) <- xs ]


exprToScheme :: Expr Ann -> AST

exprToScheme (Literal _ann literal) =
  literalToScheme literal

exprToScheme (Var _ann qualifiedIdent) =
  Identifier (showQualified runIdent qualifiedIdent)

-- `values' holds the values we're matching against, for instance it could be
-- a list of Vars where each Var is the LHS of the pattern matching. For instance
-- in `foo 1 a = 0', `1' and `a' are values.
-- `caseAlternatives' holds the various cases we're matching against such values.
-- for each case there is a binder and a possible result if the match holds.
exprToScheme (Case _ann values caseAlternatives) =
  caseToScheme values caseAlternatives

exprToScheme (Abs _ann arg expr) =
  Lambda (runIdent arg) (exprToScheme expr)

exprToScheme (App _ann function arg) =
  Application (exprToScheme function) [exprToScheme arg]

exprToScheme _ = error "Not implemented"


literalToScheme :: Literal (Expr Ann) -> AST
literalToScheme (NumericLiteral (Left integer)) = IntegerLiteral integer
literalToScheme (ArrayLiteral xs) = VectorLiteral $ fmap exprToScheme xs
literalToScheme _ = error "Not implemented"


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


caseToScheme :: [Expr Ann] -> [CaseAlternative Ann] -> AST
caseToScheme values caseAlternatives =
  Cond clauses
  where
    clauses = concatMap condClauses caseExpr
    caseExpr = straightenCaseExpr values caseAlternatives

    -- Emit clauses for a cond expression. In scheme: (cond clause1 clause2 ...)
    -- Return a list of pairs where the first element is the condition and the
    -- second element is the result.
    condClauses
      :: ([(Expr Ann, Binder Ann)], Either [(Expr Ann, Expr Ann)] (Expr Ann))
      -> [(AST, AST)]

    -- If there are guards we're provided with multiple results and each result
    -- is associated to a guard. Thus we have to emit multiple cond clauses for
    -- each result.
    condClauses (valuesAndBinders, Left guardsAndResults) =
      map (\(guard, result) ->
             (exprToScheme guard,
              replaceVariables (exprToScheme result)
                               (variablesToReplace valuesAndBinders)))
          guardsAndResults

    -- If there are no guards we're provided with a single result. In this case
    -- we can emit a single cond clause for the single result associated to
    -- multiple values and binders.
    condClauses (valuesAndBinders, Right result) = 
      [(test valuesAndBinders, bindersToResult valuesAndBinders result)]

    -- Emit the test for a cond clause. In scheme: (cond (test1 result1) ...)
    -- If we're handling multiple values and binders for a single result
    -- we have to wrap the various resulting test into an `and' expression.
    -- In scheme: (cond ((and test1a test1b) result1) ...)
    test [(value, binder)] = binderToTest value binder
    test valueAndBinders =
      Application (Identifier "and")
                  (map (\(value, binder) -> binderToTest value binder)
                       valueAndBinders)

    -- Emit a cond clause test for each value and binder.
    -- E.g.: (value: 23, binder: 42) -> (= 23 42)
    binderToTest :: Expr Ann -> Binder Ann -> AST
    binderToTest value (LiteralBinder _ann (NumericLiteral (Left integer))) =
      Application (Identifier "=") [exprToScheme value, IntegerLiteral integer]
    binderToTest _value (VarBinder _ann _ident) = Identifier "#t"
    binderToTest _value (NullBinder _ann) = Identifier "#t"
    binderToTest _ _ = error "Not implemented"

    -- Emit a cond clause result for multiple values and binders associated to
    -- a single result. This is done by replacing the identifiers in the result
    -- expression according to its VarBinders.
    bindersToResult :: [(Expr Ann, Binder a)] -> Expr Ann -> AST
    bindersToResult valuesAndBinders result =
      everywhere
        (\ast -> (replaceVariables ast (variablesToReplace valuesAndBinders)))
        (exprToScheme result)

    -- Each identifier in a VarBinder will be replaced by its corresponding
    -- value. E.g. `foo m n = m + n' will emit a function whose parameters are
    -- v and v0, but the `m + n' expression will use `m' and `n' as variable.
    -- We have to replace each occurrence of `m' with `v' and each occurrence
    -- of `n' with `v0'.
    variablesToReplace valuesAndBinders =
      [ (runIdent ident, exprToScheme value)
      | (value, (VarBinder _ann ident)) <- valuesAndBinders ]


-- Replace each (Identifier from) into a `to' AST everywhere in ast
replaceVariables :: AST -> [(Text, AST)] -> AST
replaceVariables (Identifier i) ((from, to):xs) =
  if i == from then to else replaceVariables (Identifier i) xs
replaceVariables ast [] = ast
replaceVariables ast _xs = ast


