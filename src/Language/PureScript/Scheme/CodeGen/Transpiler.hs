module Language.PureScript.Scheme.CodeGen.Transpiler where

import Language.PureScript.CoreFn.Module      (Module(..))
import Language.PureScript.CoreFn.Ann         (Ann)
import Language.PureScript.CoreFn.Expr        (Expr(..), Bind(..), Guard,
                                               CaseAlternative(..))
import Language.PureScript.CoreFn.Binders     (Binder(..))
import Language.PureScript.Names              (runIdent, showQualified)
import Language.PureScript.AST.Literals       (Literal(..))
import Language.PureScript.Scheme.CodeGen.AST (AST(..))

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

exprToScheme (Case _ann values caseAlternatives) =
  caseAlternativesToScheme values caseAlternatives

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
straightenCaseAlternatives
  :: [Expr Ann]
  -> [CaseAlternative Ann]
  -> [([(Expr Ann, Binder Ann)], Either [(Guard Ann, Expr Ann)] (Expr Ann))]
  --     ^Value^^  ^Binder^^^    ^Res^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
straightenCaseAlternatives values caseAlternatives =
  map (\(CaseAlternative binders result) -> (zip values binders, result))
      caseAlternatives


caseAlternativesToScheme :: [Expr Ann] -> [CaseAlternative Ann] -> AST
caseAlternativesToScheme values caseAlternatives =
  let alternatives = straightenCaseAlternatives values caseAlternatives
  in Cond $ concatMap (\(valuesAndBinders, result) ->
                         map (\(value, binder) ->
                                ((unifyBinder value binder),
                                 case result of
                                   Left _xs -> error "Not implemented"
                                   Right expr -> exprToScheme expr))
                             valuesAndBinders)
                      alternatives


-- Produce clauses for the cond expression.
-- (value: 23, binder: 42) -> (= 23 42)
unifyBinder :: Expr Ann -> Binder Ann -> AST

unifyBinder value (LiteralBinder _ann literal) = unifyLiteralBinder value literal

-- TODO: This is possibly a nasty hack to make Cond work.
-- Check how to properly translate VarBinder.
-- Mind that `else' is not an identifier but an auxiliary keyword and it is a
-- syntax violation to reference it outside of contexts where they are recognized
-- as such.
unifyBinder _value (VarBinder _ann _ident) = Identifier "else"

unifyBinder _ _ = error "Not implemented"


unifyLiteralBinder :: Expr Ann -> Literal (Binder Ann) -> AST

unifyLiteralBinder value (NumericLiteral (Left integer))=
  Application (Identifier "=") [exprToScheme value, IntegerLiteral integer]

unifyLiteralBinder _ _ = error "Not implemented"
