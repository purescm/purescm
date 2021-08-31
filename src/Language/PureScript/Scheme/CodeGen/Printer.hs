module Language.PureScript.Scheme.CodeGen.Printer where

import Data.Text (Text, pack, intercalate)
import Language.PureScript.PSString (prettyPrintStringJS)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..))

tshow :: Show a => a -> Text
tshow = pack . show

char :: Char -> Text
char x = "#\\" <> pack [x]

bool :: Bool -> Text
bool True = "#t"
bool False = "#f"

parens :: Text -> Text
parens x = "(" <> x <> ")"

list :: [Text] -> Text
list xs = parens $ intercalate " " xs

emit :: SExpr -> Text
emit (Integer x) = tshow x
emit (Float x) = tshow x
emit (String x) = prettyPrintStringJS x
emit (Character x) = char x
emit (Boolean x) = bool x
emit (Symbol x) = x
emit (List xs) = list $ map emit xs

printScheme :: [SExpr] -> Text
printScheme xs = intercalate "\n\n" (fmap emit xs)
