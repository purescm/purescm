module Language.PureScript.Scheme.CodeGen.Printer where

import Data.Text                              (Text, pack, intercalate)
import Language.PureScript.Scheme.CodeGen.AST (AST(..))

tshow :: Show a => a -> Text
tshow = pack . show

parens :: Text -> Text
parens x = "(" <> x <> ")"

list :: [Text] -> Text
list xs = parens $ intercalate " " xs

cond :: [Text] -> Text
cond xs = list ("cond" : xs)

application :: Text -> [Text] -> Text
application function args = list (function : args)

lambda :: Text -> Text -> Text
lambda parameter expr = list ["lambda", list [parameter], expr]

emit :: AST -> Text
emit (IntegerLiteral integer) = tshow integer
emit (Identifier x) = x
emit (Application function args) = application (emit function) (fmap emit args)
emit (Lambda arg expr) = lambda arg (emit expr)
emit (List xs) = list $ map emit xs

printScheme :: [AST] -> Text
printScheme xs = intercalate "\n\n" (fmap emit xs)
