module Language.PureScript.Scheme.CodeGen.Printer where

import Data.Text                              (Text, pack, intercalate)
import Language.PureScript.Scheme.CodeGen.AST (AST(..))

tshow :: Show a => a -> Text
tshow = pack . show

parens :: Text -> Text
parens x = "(" <> x <> ")"

list :: [Text] -> Text
list xs = parens $ intercalate " " xs

emit :: AST -> Text
emit (IntegerLiteral integer) = tshow integer
emit (Identifier x) = x
emit (List xs) = list $ map emit xs

printScheme :: [AST] -> Text
printScheme xs = intercalate "\n\n" (fmap emit xs)
