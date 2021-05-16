module Language.PureScript.Scheme.CodeGen.Optimizer where

import Language.PureScript.Scheme.CodeGen.AST (AST(..), everywhere)


-- Single pass optimizer.
-- Run through all the AST expressions and apply the optimizations.
runOptimizations :: [AST] -> [AST]
runOptimizations xs = map (\x -> everywhere specializeOperators x) xs


-- TODO: possibly nasty hack. To review once externs are implemented.
specializeOperators :: AST -> AST

specializeOperators (Application (Application (Application (Identifier "Data.Semiring.add")
                                 [Identifier "Data.Semiring.semiringInt"]) [x]) [y])
  = Application (Identifier "+") [x, y]

specializeOperators (Application (Application (Application (Identifier "Data.Ring.sub")
                                 [Identifier "Data.Ring.ringInt"]) [x]) [y])
  = Application (Identifier "-") [x, y]

specializeOperators ast = ast
