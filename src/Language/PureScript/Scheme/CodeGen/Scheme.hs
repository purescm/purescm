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
stringEqQ' = Symbol "scm:string=?"

stringHash' :: SExpr
stringHash' = Symbol "scm:string-hash"


-- Scheme special forms --------------------------------------------------------

define :: Text -> SExpr -> SExpr
define name expr = List [Symbol "scm:define", Symbol name, expr]

quote :: SExpr -> SExpr
quote x = app "scm:quote" [x]

lambda :: [Text] -> SExpr -> SExpr
lambda formals expr =
  List [Symbol "scm:lambda", List $ map Symbol formals, expr]

lambda1 :: Text -> SExpr -> SExpr
lambda1 formal expr = lambda [formal] expr

letRecS :: [(Text, SExpr)] -> SExpr -> SExpr
letRecS bodies expr = List [Symbol "scm:letrec*", List $ fmap go bodies, expr]
  where
    go :: (Text, SExpr) -> SExpr
    go (var, expr') = List [Symbol var, expr']

letRecS1 :: (Text, SExpr) -> SExpr -> SExpr
letRecS1 body expr = letRecS [body] expr

condWithElse_ :: [(SExpr, SExpr)] -> Maybe SExpr -> SExpr
condWithElse_ clauses maybeElse
  = app "scm:cond" clausesWithElse
  where
    clauses' :: [SExpr]
    clauses' = map (\(test, expr) -> List [test, expr]) clauses

    clausesWithElse :: [SExpr]
    clausesWithElse = clauses' <> [List [Symbol "scm:else", elseExpr]]

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
begin xs = app "scm:begin" xs


-- Scheme functions ------------------------------------------------------------

eq :: [SExpr] -> SExpr
eq xs = app "scm:=" xs

eq2 :: SExpr -> SExpr -> SExpr
eq2 x y = eq [x, y]

eqQ :: SExpr -> SExpr -> SExpr
eqQ x y = app "scm:eq?" [x, y]

charEqQ :: [SExpr] -> SExpr
charEqQ xs = app "scm:char=?" xs

charEqQ2 :: SExpr -> SExpr -> SExpr
charEqQ2 x y = charEqQ [x, y]

stringEqQ :: [SExpr] -> SExpr
stringEqQ xs = app "scm:string=?" xs

stringEqQ2 :: SExpr -> SExpr -> SExpr
stringEqQ2 s1 s2 = stringEqQ [s1, s2]

and_ :: [SExpr] -> SExpr
and_ xs = app "scm:and" xs

cons :: SExpr -> SExpr -> SExpr
cons x y = app "scm:cons" [x, y]

car :: SExpr -> SExpr
car l = app "scm:car" [l]

cdr :: SExpr -> SExpr
cdr l = app "scm:cdr" [l]

stringHash :: SExpr -> SExpr
stringHash string = app "scm:string-hash" [string]

vector :: [SExpr] -> SExpr
vector xs = app "scm:vector" xs

vectorLength :: SExpr -> SExpr
vectorLength x = app "scm:vector-length" [x]

vectorRef :: SExpr -> SExpr -> SExpr
vectorRef v i = app "scm:vector-ref" [v, i]

-- | (make-hashtable hash equiv?)
-- hash and equiv? must be procedures. If size is provided, it must be a
-- nonnegative exact integer indicating approximately how many elements
-- the hashtable should initially hold.
makeHashtable :: SExpr -> SExpr -> SExpr -> SExpr
makeHashtable hash equivQ size = app "scm:make-hashtable" [hash, equivQ, size]

hashtableSetB :: SExpr -> SExpr -> SExpr -> SExpr
hashtableSetB hashtable key obj = app "scm:hashtable-set!" [hashtable, key, obj]

hashtableRef :: SExpr -> SExpr -> SExpr -> SExpr
hashtableRef hashtable key default_
  = app "scm:hashtable-ref" [hashtable, key, default_]

hashtableCopy :: SExpr -> SExpr
hashtableCopy hashtable = app "scm:hashtable-copy" [hashtable]

errorWithWhoAndIrritants :: SExpr -> SExpr -> [SExpr] -> SExpr
errorWithWhoAndIrritants who msg irritants
  = app "scm:error" $ [who, msg] <> irritants

error_ :: SExpr -> SExpr
error_ msg = errorWithWhoAndIrritants f msg []
