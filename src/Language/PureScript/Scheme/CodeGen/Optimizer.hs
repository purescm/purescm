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
  | klass == "Data.Semiring.semiringInt" && op == "Data.Semiring.add"
  = app "scm:+" [x, y]
  | klass == "Data.Ring.ringInt" && op == "Data.Ring.sub"
  = app "scm:-" [x, y]
  | klass == "Data.Ring.ringNumber" && op == "Data.Ring.sub"
  = app "scm:-" [x, y]
  | klass == "Data.Semiring.semiringNumber" && op == "Data.Semiring.add"
  = app "scm:+" [x, y]
  | klass == "Data.Semiring.semiringNumber" && op == "Data.Semiring.mul"
  = app "scm:*" [x, y]
  | klass == "Data.EuclideanRing.euclideanRingNumber"
    && op == "Data.EuclideanRing.div"
  = app "scm:/" [x, y]
  | klass == "Data.Ord.ordNumber" && op == "Data.Ord.lessThan"
  = app "scm:<" [x, y]

inlineCommonBinaryOperators expr = expr


-- Reduce (and #t #t #t) to #t
-- Reduce (and x #t y) to (and x y)
-- Reduce (and x) to x
-- TODO: write a test
simplifyLogic :: SExpr -> SExpr

simplifyLogic (List ((Symbol "scm:and") : args)) =
  let
    go ((Symbol "#t") : xs) = go xs
    go (x : xs) = (x : go xs)
    go [] = []
  in
    case go args of
      [] -> Symbol "#t"
      [x]  -> x
      xs -> app "scm:and" xs

simplifyLogic other = other
