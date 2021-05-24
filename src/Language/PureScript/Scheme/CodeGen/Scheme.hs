module Language.PureScript.Scheme.CodeGen.Scheme where

import Language.PureScript.Scheme.CodeGen.AST (AST(..))


vector :: [AST] -> AST
vector xs = Application (Identifier "vector") xs
