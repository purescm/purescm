module Language.PureScript.Scheme.CodeGen.Optimizer where

import Language.PureScript.Scheme.CodeGen.AST (AST(..), everywhere)


optimizations :: AST -> AST
optimizations = (simplifyLogic . specializeOperators)


-- Single pass optimizer.
-- Run through all the AST expressions and apply the optimizations.
runOptimizations :: [AST] -> [AST]
runOptimizations xs = map (\x -> everywhere optimizations x) xs


-- TODO: possibly nasty hack. To review once externs are implemented.
specializeOperators :: AST -> AST

specializeOperators (Application (Application (Application (Identifier "Data.Semiring.add")
                                 [Identifier "Data.Semiring.semiringInt"]) [x]) [y])
  = Application (Identifier "+") [x, y]

specializeOperators (Application (Application (Application (Identifier "Data.Ring.sub")
                                 [Identifier "Data.Ring.ringInt"]) [x]) [y])
  = Application (Identifier "-") [x, y]

specializeOperators ast = ast


-- Reduce (and #t #t #t) to #t
-- Reduce (and x #t y) to (and x y)
-- Reduce (and x) to x
-- TODO: write a test
simplifyLogic :: AST -> AST
simplifyLogic (Application (Identifier "and") args) =
  let
    go ((Identifier "#t") : xs) = go xs
    go (x : xs) = (x : go xs)
    go [] = []
  in
    case go args of
      [] -> Identifier "#t"
      [x]  -> x
      xs -> (Application (Identifier "and") xs)

simplifyLogic other = other
