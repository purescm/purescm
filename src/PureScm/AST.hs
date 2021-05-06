module PureScm.AST where

import qualified Data.Text                        as T
import           Data.Text                        (Text)
import qualified Data.List                        as L
import qualified Language.PureScript.PSString     as PSString
import           Language.PureScript.PSString     (PSString)
import           Language.PureScript.AST.Literals (Literal(..))

showT :: Show a => a -> Text
showT = T.pack . show

-- TODO: Expr must have also:
--       - Accessor (should map to Scheme's hashmap-get)
--       - Let (recursive and non-recursive)
--       - Case (could be mapped to Scheme's cond)
--       - ObjectUpdate (do later)
--       - Constructor (do later)
data Expr =
    Symbol String
  | Literal (Literal Expr)
  | Lambda Text Expr
  | Application Expr Expr
  deriving Show

-- TODO: In scheme we're using strings as keys.
--       Check if using symbols instead of strings can give us some benefit
--       e.g. better performance.
makeObject :: [(PSString, Expr)] -> Text
makeObject xs =
     "(let ([ht (make-hashtable string-hash string=?)]) "
  <> foldMap (\(k, v) -> "(hashtable-set! ht "
                      <> showT k <> " " <> render v
                      <> ") ")
             xs
  <> "ht)"

render :: Expr -> Text

render (Symbol s) = T.pack s

render (Literal literal) = case literal of
  NumericLiteral (Left integer) -> showT integer
  NumericLiteral (Right double) -> showT double
  StringLiteral  psString       -> case PSString.decodeString psString of
    Just s -> s
    Nothing -> ""
  CharLiteral    char           -> "#\\" <> T.singleton char
  -- ^ TODO: escape char, check purerl
  BooleanLiteral True           -> "#t"
  BooleanLiteral False          -> "#f"
  ArrayLiteral   xs             -> "#(" <> (T.intercalate " " $ map render xs) <> ")"
  ObjectLiteral  kvs            -> makeObject kvs


render (Lambda param expr) =
     "(lambda (" <> param <> ") " <> render expr <> ")"

render (Application fn arg) =
     "(" <> render fn <> " " <> render arg <> ")"
