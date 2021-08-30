module Language.PureScript.Scheme.CodeGen.Scheme where

import Data.Text (Text)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..))


-- Helpers ---------------------------------------------------------------------

app :: Text -> [SExpr] -> SExpr
app name args = List ((Symbol name):args)


-- Scheme symbols --------------------------------------------------------------

t :: SExpr
t = Symbol "#t"

f :: SExpr
f = Symbol "#f"

stringEqQ' :: SExpr
stringEqQ' = Symbol "string=?"

stringHash' :: SExpr
stringHash' = Symbol "string-hash"


-- Scheme special forms --------------------------------------------------------

define :: Text -> SExpr -> SExpr
define name expr = List [Symbol "define", Symbol name, expr]

quote :: SExpr -> SExpr
quote x = app "quote" [x]

lambda :: [Text] -> SExpr -> SExpr
lambda formals expr =
  List [Symbol "lambda", List $ map Symbol formals, expr]

lambda1 :: Text -> SExpr -> SExpr
lambda1 formal expr = lambda [formal] expr

let_ :: [(Text, SExpr)] -> SExpr -> SExpr
let_ bodies expr = List [Symbol "let", List $ fmap go bodies, expr]
  where
    go :: (Text, SExpr) -> SExpr
    go (var, expr') = List [Symbol var, expr']

let1 :: (Text, SExpr) -> SExpr -> SExpr
let1 body expr = let_ [body] expr

condWithElse_ :: [(SExpr, SExpr)] -> Maybe SExpr -> SExpr
condWithElse_ clauses maybeElse
  = app "cond" clausesWithElse
  where
    clauses' :: [SExpr]
    clauses' = map (\(test, expr) -> List [test, expr]) clauses

    clausesWithElse :: [SExpr]
    clausesWithElse = clauses' <> [List [Symbol "else", elseExpr]]

    elseExpr :: SExpr
    elseExpr = case maybeElse of
      Just elseExpr' -> elseExpr'
                        -- TODO: add sourcespan
      Nothing -> error_ (String "Failed pattern match")

condWithElse :: [(SExpr, SExpr)] -> SExpr -> SExpr
condWithElse clauses elseExpr = condWithElse_ clauses (Just elseExpr)

cond :: [(SExpr, SExpr)] -> SExpr
cond clauses = condWithElse_ clauses Nothing 

begin :: [SExpr] -> SExpr
begin xs = app "begin" xs


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

cons :: SExpr -> SExpr -> SExpr
cons x y = app "cons" [x, y]

car :: SExpr -> SExpr
car l = app "car" [l]

cdr :: SExpr -> SExpr
cdr l = app "cdr" [l]

stringHash :: SExpr -> SExpr
stringHash string = app "string-hash" [string]

vector :: [SExpr] -> SExpr
vector xs = app "vector" xs

vectorRef :: SExpr -> SExpr -> SExpr
vectorRef v i = app "vector-ref" [v, i]

-- | (make-hashtable hash equiv?)
-- hash and equiv? must be procedures. If size is provided, it must be a
-- nonnegative exact integer indicating approximately how many elements
-- the hashtable should initially hold.
makeHashtable :: SExpr -> SExpr -> SExpr -> SExpr
makeHashtable hash equivQ size = app "make-hashtable" [hash, equivQ, size]

hashtableSetB :: SExpr -> SExpr -> SExpr -> SExpr
hashtableSetB hashtable key obj = app "hashtable-set!" [hashtable, key, obj]

hashtableRef :: SExpr -> SExpr -> SExpr -> SExpr
hashtableRef hashtable key default_
  = app "hashtable-ref" [hashtable, key, default_]

hashtableCopy :: SExpr -> SExpr
hashtableCopy hashtable = app "hashtable-copy" [hashtable]

errorWithWhoAndIrritants :: SExpr -> SExpr -> [SExpr] -> SExpr
errorWithWhoAndIrritants who msg irritants
  = app "error" $ [who, msg] <> irritants

error_ :: SExpr -> SExpr
error_ msg = errorWithWhoAndIrritants f msg []
