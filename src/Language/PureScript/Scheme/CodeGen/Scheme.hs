module Language.PureScript.Scheme.CodeGen.Scheme where

import Data.Text                              (Text)
import Language.PureScript.Scheme.CodeGen.AST (AST(..))


-- Helpers ---------------------------------------------------------------------

app :: Text -> [AST] -> AST
app name args = Application (Identifier name) args


-- Scheme symbols --------------------------------------------------------------

t :: AST
t = Identifier "#t"


-- Scheme functions ------------------------------------------------------------

eq :: [AST] -> AST
eq xs = app "=" xs

eqQ :: AST -> AST -> AST
eqQ x y = app "eq?" [x, y]

and_ :: [AST] -> AST
and_ xs = app "and" xs

quote :: AST -> AST
quote x = app "quote" [x]

cons :: AST -> AST -> AST
cons x y = app "cons" [x, y]

car :: AST -> AST
car l = app "car" [l]

cdr :: AST -> AST
cdr l = app "cdr" [l]

-- (cond (test expr) ... (else expr))
cond :: [(AST, AST)] -> AST -> AST
cond clauses elseExpr = cond' (clauses ++ [(Identifier "else", elseExpr)])

-- (cond (test expr) ...)
cond' :: [(AST, AST)] -> AST
cond' clauses = app "cond" $ map (\(test, expr) -> List [test, expr]) clauses

vector :: [AST] -> AST
vector xs = app "vector" xs

vectorRef :: AST -> AST -> AST
vectorRef v i = app "vector-ref" [v, i]
