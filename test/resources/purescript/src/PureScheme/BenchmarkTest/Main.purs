module PureScheme.BenchmarkTest.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (withUnits, benchWith')

import PureScheme.BenchmarkTest.Fibonacci as Fibonacci
import PureScheme.BenchmarkTest.FibonacciTco as FibonacciTco
import PureScheme.BenchmarkTest.Ackermann as Ackermann
import PureScheme.BenchmarkTest.AckermannCps as AckermannCps
import PureScheme.BenchmarkTest.RecordUpdate as RecordUpdate

bench
  :: forall a
   . String
  -> Int
  -> (Unit -> a)
  -> Effect Unit
bench label n f = do
  res <- benchWith' n f
  log $ label <> ": { "
    <> "mean = "   <> withUnits res.mean   <> " | "
    <> "stddev = " <> withUnits res.stdDev <> " | "
    <> "min = "    <> withUnits res.min    <> " | "
    <> "max = "    <> withUnits res.max
    <> " }"

main :: Effect Unit
main = do
  -- Fibonacci --
  bench "Fibonacci  5" 1000 (\_ -> Fibonacci.test 5)
  bench "Fibonacci 10" 1000 (\_ -> Fibonacci.test 10)
  bench "Fibonacci 15" 1000 (\_ -> Fibonacci.test 15)
  bench "Fibonacci 20" 1000 (\_ -> Fibonacci.test 20)

  -- Fibonacci TCO --
  bench "FibonacciTco  5" 1000 (\_ -> FibonacciTco.test 5)
  bench "FibonacciTco 10" 1000 (\_ -> FibonacciTco.test 10)
  bench "FibonacciTco 15" 1000 (\_ -> FibonacciTco.test 15)
  bench "FibonacciTco 20" 1000 (\_ -> FibonacciTco.test 20)

  -- Ackermann --
  bench "Ackermann 2 3" 1000 (\_ -> Ackermann.test 2 3)
  bench "Ackermann 2 4" 1000 (\_ -> Ackermann.test 2 4)
  bench "Ackermann 3 1" 1000 (\_ -> Ackermann.test 3 1)
  bench "Ackermann 3 2" 1000 (\_ -> Ackermann.test 3 2)

  -- Ackermann CPS --
  bench "AckermannCps 2 3" 1000 (\_ -> AckermannCps.test 2 3)
  bench "AckermannCps 2 4" 1000 (\_ -> AckermannCps.test 2 4)
  bench "AckermannCps 3 1" 1000 (\_ -> AckermannCps.test 3 1)
  bench "AckermannCps 3 2" 1000 (\_ -> AckermannCps.test 3 2)

  -- Record update --
  bench "RecordUpdate    1000" 1000 (\_ -> RecordUpdate.test 1000)
  bench "RecordUpdate   10000" 1000 (\_ -> RecordUpdate.test 10000)
  bench "RecordUpdate  100000" 1000 (\_ -> RecordUpdate.test 100000)
  bench "RecordUpdate 1000000" 1000 (\_ -> RecordUpdate.test 1000000)
