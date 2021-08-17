module Language.PureScript.Scheme.CodeGen.Scheme where

import Data.Text (Text)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..))


-- Helpers ---------------------------------------------------------------------

app :: Text -> [SExpr] -> SExpr
app name args = List ((Symbol name):args)


-- Scheme symbols --------------------------------------------------------------

t :: SExpr
t = Symbol "#t"


-- Scheme special forms --------------------------------------------------------

define :: Text -> SExpr -> SExpr
define name expr = List [Symbol "define", Symbol name, expr]

lambda :: [Text] -> SExpr -> SExpr
lambda formals expr =
  List [Symbol "lambda", List $ map Symbol formals, expr]

lambda1 :: Text -> SExpr -> SExpr
lambda1 formal expr = lambda [formal] expr


-- Scheme functions ------------------------------------------------------------

eq :: [SExpr] -> SExpr
eq xs = app "=" xs

eq2 :: SExpr -> SExpr -> SExpr
eq2 x y = eq [x, y]

eqQ :: SExpr -> SExpr -> SExpr
eqQ x y = app "eq?" [x, y]

stringEqQ :: [SExpr] -> SExpr
stringEqQ xs = app "string=?" xs

stringEqQ2 :: SExpr -> SExpr -> SExpr
stringEqQ2 s1 s2 = stringEqQ [s1, s2]

and_ :: [SExpr] -> SExpr
and_ xs = app "and" xs

quote :: SExpr -> SExpr
quote x = app "quote" [x]

cons :: SExpr -> SExpr -> SExpr
cons x y = app "cons" [x, y]

car :: SExpr -> SExpr
car l = app "car" [l]

cdr :: SExpr -> SExpr
cdr l = app "cdr" [l]

-- (cond (test expr) ... (else expr))
cond :: [(SExpr, SExpr)] -> SExpr -> SExpr
cond clauses elseExpr = cond' (clauses ++ [(Symbol "else", elseExpr)])

-- (cond (test expr) ...)
cond' :: [(SExpr, SExpr)] -> SExpr
cond' clauses = app "cond" $ map (\(test, expr) -> List [test, expr]) clauses

vector :: [SExpr] -> SExpr
vector xs = app "vector" xs

vectorRef :: SExpr -> SExpr -> SExpr
vectorRef v i = app "vector-ref" [v, i]

errorWithIrritants :: SExpr -> SExpr -> [SExpr] -> SExpr
errorWithIrritants who msg irritants = app "error" $ [who, msg] <> irritants

error_ :: SExpr -> SExpr -> SExpr
error_ who msg = errorWithIrritants who msg []
