module Language.PureScript.Scheme.CodeGen.Optimizer where

import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..), everywhere)
import Language.PureScript.Scheme.CodeGen.Scheme (app)


-- Single pass optimizer.
-- Run through all the SExpr expressions and apply the optimizations.
runOptimizations :: [SExpr] -> [SExpr]
runOptimizations xs = map (\x -> everywhere optimizations x) xs


optimizations :: SExpr -> SExpr
optimizations = (simplifyLogic . inlineCommonBinaryOperators)


inlineCommonBinaryOperators :: SExpr -> SExpr

inlineCommonBinaryOperators (List [List [List [Symbol op, Symbol klass], x], y])
  | op == "Data.Semiring.add" && klass == "Data.Semiring.semiringInt" =
    app "scm:+" [x, y]
  | op == "Data.Ring.sub" && klass == "Data.Ring.ringInt" =
    app "scm:-" [x, y]

inlineCommonBinaryOperators expr = expr


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
