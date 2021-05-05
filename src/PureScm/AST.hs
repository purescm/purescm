module PureScm.AST where

import qualified Data.List as L
import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.Literals (Literal(..))

-- TODO: Expr must have also:
--       - Accessor (should map to Scheme's hashmap-get)
--       - Let (recursive and non-recursive)
--       - Case (could be mapped to Scheme's cond)
--       - ObjectUpdate (do later)
--       - Constructor (do later)
data Expr =
    Symbol String
  | Literal (Literal Expr)
  | Lambda String Expr
  | Application Expr Expr
  deriving Show

-- TODO: In scheme we're using strings as keys.
--       Check if using symbols instead of strings can give us some benefit
--       e.g. better performance.
makeObject :: [(PSString, Expr)] -> String
makeObject xs =
     "(let ([ht (make-hashtable string-hash string=?)]) "
  <> foldMap (\(k, v) -> "(hashtable-set! ht "
                      <> show k <> " " <> render v
                      <> ") ")
             xs
  <> "ht)"

render :: Expr -> String

render (Symbol s) = s

render (Literal literal) = case literal of
  NumericLiteral (Left integer) -> show $ integer
  NumericLiteral (Right double) -> show $ double
  StringLiteral  psString       -> show $ psString
  CharLiteral    char           -> "#\\" <> [char]  -- TODO: escape char, check purerl
  BooleanLiteral True           -> "#t"
  BooleanLiteral False          -> "#f"
  ArrayLiteral   xs             -> "#(" <> (L.intercalate " " $ map render xs) <> ")"
  ObjectLiteral  kvs            -> makeObject kvs


render (Lambda param expr) =
     "(lambda (" <> param <> ") " <> render expr <> ")"

render (Application fn arg) =
     "(" <> render fn <> " " <> render arg <> ")"
