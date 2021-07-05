module Language.PureScript.Scheme.CodeGen.Optimizer where

import Language.PureScript.Scheme.CodeGen.AST (AST(..), everywhere)
import Language.PureScript.Scheme.CodeGen.Scheme (app)


optimizations :: AST -> AST
optimizations = (simplifyLogic . specializeOperators)


-- Single pass optimizer.
-- Run through all the AST expressions and apply the optimizations.
runOptimizations :: [AST] -> [AST]
runOptimizations xs = map (\x -> everywhere optimizations x) xs


-- TODO: possibly nasty hack. To review once externs are implemented.
specializeOperators :: AST -> AST

specializeOperators (List [List [List [Identifier "Data.Semiring.add",
                                       Identifier "Data.Semiring.semiringInt"],
                           x], y])
  = app "+" [x, y]

specializeOperators (List [List [List [Identifier "Data.Ring.sub",
                                       Identifier "Data.Ring.ringInt"],
                                  x], y])
  = app "-" [x, y]

specializeOperators ast = ast


-- Reduce (and #t #t #t) to #t
-- Reduce (and x #t y) to (and x y)
-- Reduce (and x) to x
-- TODO: write a test
simplifyLogic :: AST -> AST
simplifyLogic (List ((Identifier "and"):args)) =
  let
    go ((Identifier "#t") : xs) = go xs
    go (x : xs) = (x : go xs)
    go [] = []
  in
    case go args of
      [] -> Identifier "#t"
      [x]  -> x
      xs -> app "and" xs

simplifyLogic other = other
