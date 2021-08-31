module Language.PureScript.Scheme.CodeGen.Printer where

import Data.Text (Text, pack, intercalate)
import Language.PureScript.PSString (prettyPrintStringJS)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..))
import Language.PureScript.Scheme.CodeGen.Library (Library(..))

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

printSExprs :: [SExpr] -> Text
printSExprs exprs = intercalate "\n\n" (fmap emit exprs)

printLibrary :: Library -> Text
printLibrary (Library name exports imports body)
  = list [ "library"
         , list [ name, "lib" ]
         , list $ "export" : exports
         , list $ [ "import"
                  , list [ "rnrs" ]
                  ] ++ map (\n -> (list [ "prefix"
                                        , list [ n, "lib" ]
                                        , n <> "."]))
                           imports
         , printSExprs body
         ]
