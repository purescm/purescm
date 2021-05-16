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
  fmap bindToScheme declarations


-- TODO: CoreFn uses Bind both for top-level definitions and let bindings so
-- it is very likely that this will break once let expressions are translated.
bindToScheme :: Bind Ann -> AST

bindToScheme (NonRec _ann ident expr) =
  Define (runIdent ident) (exprToScheme expr)

-- TODO: handle multiple definitions
-- For top-level definitions mapping each (ident, expr) to define form should be
-- enough since Scheme can handle recursive defines.
-- For let bindings it may be the case to determine what to use between let,
-- let*, letrec and letrec*.
-- In both cases we should have to change the return signature from AST to [AST].
bindToScheme (Rec xs) =
  let ((_ann, ident), expr) = xs !! 0
  in Define (runIdent ident) (exprToScheme expr)


exprToScheme :: Expr Ann -> AST

exprToScheme (Literal _ann literal) =
  literalToScheme literal

exprToScheme (Var _ann qualifiedIdent) =
  Identifier (showQualified runIdent qualifiedIdent)

-- values holds the values we're matching against, for instance it could be
-- a list of Vars where each Var is the LHS of the pattern matching.
-- caseAlternatives holds the various cases we're matching against such values.
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
    caseExpr = straightenCaseExpr values caseAlternatives

    clauses = map clause caseExpr

    -- (cond clause1, clause2, ...)
    clause (_valuesAndBinders, Left _xs) = error "Not implemented"
    clause (valuesAndBinders, Right expr) = 
      (test valuesAndBinders, result valuesAndBinders expr)

    -- (cond (test1 result1) (test2 result2) ...)
    -- If we're handling multiple values and binders for a single result
    -- we have to wrap each one inside an `and' form.
    test [(value, binder)] = binderToTest value binder
    test valueAndBinders =
      Application (Identifier "and")
                  (map (\(value, binder) -> binderToTest value binder)
                       valueAndBinders)

    -- Produce clauses for the cond expression.
    -- (value: 23, binder: 42) -> (= 23 42)
    binderToTest :: Expr Ann -> Binder Ann -> AST
    binderToTest value (LiteralBinder _ann (NumericLiteral (Left integer))) =
      Application (Identifier "=") [exprToScheme value, IntegerLiteral integer]
    binderToTest _value (VarBinder _ann _ident) = Identifier "#t"
    binderToTest _value (NullBinder _ann) = Identifier "#t"
    binderToTest _ _ = error "Not implemented"

    result :: [(Expr Ann, Binder a)] -> Expr Ann -> AST
    result valuesAndBinders expr =
      everywhere (\ast -> (replaceVariables ast variablesToReplace))
                          (exprToScheme expr)
      where
        variablesToReplace = [ (runIdent ident, exprToScheme value)
                             | (value, (VarBinder _ann ident)) <- valuesAndBinders
                             ]


-- Replace each (Identifier from) into a `to' AST everywhere in ast
replaceVariables :: AST -> [(Text, AST)] -> AST
replaceVariables (Identifier i) ((from, to):xs) =
  if i == from then to else replaceVariables (Identifier i) xs
replaceVariables ast [] = ast
replaceVariables ast _xs = ast
