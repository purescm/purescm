module Language.PureScript.Scheme.CodeGen.Printer where

import Data.Text                              (Text, pack, intercalate)
import Language.PureScript.Scheme.CodeGen.AST (AST(..))

showT :: Show a => a -> Text
showT x = pack $ show x

parens :: Text -> Text
parens x = "(" <> x <> ")"

list :: [Text] -> Text
list xs = parens $ intercalate " " xs

define :: Text -> Text -> Text
define name expr = list ["define", name, expr]

vector :: [Text] -> Text
vector xs = list ("vector" : xs)

emit :: AST -> Text
emit (IntegerLiteral integer) = showT integer
emit (VectorLiteral xs) = vector (fmap emit xs)
emit (Define name expr) = define name (emit expr)

printScheme :: [AST] -> Text
printScheme xs = intercalate "\n\n" (fmap emit xs)
