module Language.PureScript.Scheme.CodeGen.Optimizer where

import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..), everywhere)
import Language.PureScript.Scheme.CodeGen.Scheme (app)


optimizations :: SExpr -> SExpr
optimizations = (simplifyLogic . specializeOperators)


-- Single pass optimizer.
-- Run through all the SExpr expressions and apply the optimizations.
runOptimizations :: [SExpr] -> [SExpr]
runOptimizations xs = map (\x -> everywhere optimizations x) xs


-- TODO: possibly nasty hack. To review once externs are implemented.
specializeOperators :: SExpr -> SExpr

specializeOperators (List [List [List [Symbol "Data.Semiring.add",
                                       Symbol "Data.Semiring.semiringInt"],
                           x], y])
  = app "+" [x, y]

specializeOperators (List [List [List [Symbol "Data.Ring.sub",
                                       Symbol "Data.Ring.ringInt"],
                                  x], y])
  = app "-" [x, y]

specializeOperators ast = ast


-- Reduce (and #t #t #t) to #t
-- Reduce (and x #t y) to (and x y)
-- Reduce (and x) to x
-- TODO: write a test
simplifyLogic :: SExpr -> SExpr
simplifyLogic (List ((Symbol "and"):args)) =
  let
    go ((Symbol "#t") : xs) = go xs
    go (x : xs) = (x : go xs)
    go [] = []
  in
    case go args of
      [] -> Symbol "#t"
      [x]  -> x
      xs -> app "and" xs

simplifyLogic other = other
