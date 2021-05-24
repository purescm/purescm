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

vector :: [AST] -> AST
vector xs = app "vector" xs

vectorRef :: AST -> AST -> AST
vectorRef v i = app "vector-ref" [v, i]
