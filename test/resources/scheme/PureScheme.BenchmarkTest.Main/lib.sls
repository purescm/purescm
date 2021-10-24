(library
  (PureScheme.BenchmarkTest.Main lib)
  (export bench main)
  (import
    (prefix (rnrs) scm:)
    (prefix (Control.Bind lib) Control.Bind.)
    (prefix (Data.Function lib) Data.Function.)
    (prefix (Data.Semigroup lib) Data.Semigroup.)
    (prefix (Effect lib) Effect.)
    (prefix (Effect.Console lib) Effect.Console.)
    (prefix (Performance.Minibench lib) Performance.Minibench.)
    (prefix (Prelude lib) Prelude.)
    (prefix
      (PureScheme.BenchmarkTest.Ackermann lib)
      PureScheme.BenchmarkTest.Ackermann.)
    (prefix
      (PureScheme.BenchmarkTest.AckermannCps lib)
      PureScheme.BenchmarkTest.AckermannCps.)
    (prefix
      (PureScheme.BenchmarkTest.Fibonacci lib)
      PureScheme.BenchmarkTest.Fibonacci.)
    (prefix
      (PureScheme.BenchmarkTest.FibonacciTco lib)
      PureScheme.BenchmarkTest.FibonacciTco.)
    (prefix
      (PureScheme.BenchmarkTest.ListInsert lib)
      PureScheme.BenchmarkTest.ListInsert.)
    (prefix
      (PureScheme.BenchmarkTest.RecordUpdate lib)
      PureScheme.BenchmarkTest.RecordUpdate.))


  (scm:define
    bench
    (scm:lambda
      (label)
      (scm:lambda
        (n)
        (scm:lambda
          (f)
          (((Control.Bind.bind Effect.bindEffect)
              ((Performance.Minibench.benchWith$prime n) f))
            (scm:lambda
              (res)
              ((Data.Function.apply Effect.Console.log)
                (((Data.Semigroup.append Data.Semigroup.semigroupString) label)
                  (((Data.Semigroup.append Data.Semigroup.semigroupString)
                      ": { ")
                    (((Data.Semigroup.append Data.Semigroup.semigroupString)
                        "mean = ")
                      (((Data.Semigroup.append Data.Semigroup.semigroupString)
                          (Performance.Minibench.withUnits
                            (scm:hashtable-ref
                              res
                              "mean"
                              "Key not found: mean")))
                        (((Data.Semigroup.append Data.Semigroup.semigroupString)
                            " | ")
                          (((Data.Semigroup.append
                                Data.Semigroup.semigroupString)
                              "stddev = ")
                            (((Data.Semigroup.append
                                  Data.Semigroup.semigroupString)
                                (Performance.Minibench.withUnits
                                  (scm:hashtable-ref
                                    res
                                    "stdDev"
                                    "Key not found: stdDev")))
                              (((Data.Semigroup.append
                                    Data.Semigroup.semigroupString)
                                  " | ")
                                (((Data.Semigroup.append
                                      Data.Semigroup.semigroupString)
                                    "min = ")
                                  (((Data.Semigroup.append
                                        Data.Semigroup.semigroupString)
                                      (Performance.Minibench.withUnits
                                        (scm:hashtable-ref
                                          res
                                          "min"
                                          "Key not found: min")))
                                    (((Data.Semigroup.append
                                          Data.Semigroup.semigroupString)
                                        " | ")
                                      (((Data.Semigroup.append
                                            Data.Semigroup.semigroupString)
                                          "max = ")
                                        (((Data.Semigroup.append
                                              Data.Semigroup.semigroupString)
                                            (Performance.Minibench.withUnits
                                              (scm:hashtable-ref
                                                res
                                                "max"
                                                "Key not found: max")))
                                          " }"))))))))))))))))))))

  (scm:define
    main
    ((((Control.Bind.discard Control.Bind.discardUnit) Effect.bindEffect)
        (((bench "Fibonacci  5") 1000)
          (scm:lambda (v) (PureScheme.BenchmarkTest.Fibonacci.test 5))))
      (scm:lambda
        ($__unused)
        ((((Control.Bind.discard Control.Bind.discardUnit) Effect.bindEffect)
            (((bench "Fibonacci 10") 1000)
              (scm:lambda (v) (PureScheme.BenchmarkTest.Fibonacci.test 10))))
          (scm:lambda
            ($__unused)
            ((((Control.Bind.discard Control.Bind.discardUnit)
                  Effect.bindEffect)
                (((bench "Fibonacci 15") 1000)
                  (scm:lambda
                    (v)
                    (PureScheme.BenchmarkTest.Fibonacci.test 15))))
              (scm:lambda
                ($__unused)
                ((((Control.Bind.discard Control.Bind.discardUnit)
                      Effect.bindEffect)
                    (((bench "Fibonacci 20") 1000)
                      (scm:lambda
                        (v)
                        (PureScheme.BenchmarkTest.Fibonacci.test 20))))
                  (scm:lambda
                    ($__unused)
                    ((((Control.Bind.discard Control.Bind.discardUnit)
                          Effect.bindEffect)
                        (((bench "FibonacciTco  5") 1000)
                          (scm:lambda
                            (v)
                            (PureScheme.BenchmarkTest.FibonacciTco.test 5))))
                      (scm:lambda
                        ($__unused)
                        ((((Control.Bind.discard Control.Bind.discardUnit)
                              Effect.bindEffect)
                            (((bench "FibonacciTco 10") 1000)
                              (scm:lambda
                                (v)
                                (PureScheme.BenchmarkTest.FibonacciTco.test
                                  10))))
                          (scm:lambda
                            ($__unused)
                            ((((Control.Bind.discard Control.Bind.discardUnit)
                                  Effect.bindEffect)
                                (((bench "FibonacciTco 15") 1000)
                                  (scm:lambda
                                    (v)
                                    (PureScheme.BenchmarkTest.FibonacciTco.test
                                      15))))
                              (scm:lambda
                                ($__unused)
                                ((((Control.Bind.discard
                                        Control.Bind.discardUnit)
                                      Effect.bindEffect)
                                    (((bench "FibonacciTco 20") 1000)
                                      (scm:lambda
                                        (v)
                                        (PureScheme.BenchmarkTest.FibonacciTco.test
                                          20))))
                                  (scm:lambda
                                    ($__unused)
                                    ((((Control.Bind.discard
                                            Control.Bind.discardUnit)
                                          Effect.bindEffect)
                                        (((bench "Ackermann 2 3") 1000)
                                          (scm:lambda
                                            (v)
                                            ((PureScheme.BenchmarkTest.Ackermann.test
                                                2)
                                              3))))
                                      (scm:lambda
                                        ($__unused)
                                        ((((Control.Bind.discard
                                                Control.Bind.discardUnit)
                                              Effect.bindEffect)
                                            (((bench "Ackermann 2 4") 1000)
                                              (scm:lambda
                                                (v)
                                                ((PureScheme.BenchmarkTest.Ackermann.test
                                                    2)
                                                  4))))
                                          (scm:lambda
                                            ($__unused)
                                            ((((Control.Bind.discard
                                                    Control.Bind.discardUnit)
                                                  Effect.bindEffect)
                                                (((bench "Ackermann 3 1") 1000)
                                                  (scm:lambda
                                                    (v)
                                                    ((PureScheme.BenchmarkTest.Ackermann.test
                                                        3)
                                                      1))))
                                              (scm:lambda
                                                ($__unused)
                                                ((((Control.Bind.discard
                                                        Control.Bind.discardUnit)
                                                      Effect.bindEffect)
                                                    (((bench "Ackermann 3 2")
                                                        1000)
                                                      (scm:lambda
                                                        (v)
                                                        ((PureScheme.BenchmarkTest.Ackermann.test
                                                            3)
                                                          2))))
                                                  (scm:lambda
                                                    ($__unused)
                                                    ((((Control.Bind.discard
                                                            Control.Bind.discardUnit)
                                                          Effect.bindEffect)
                                                        (((bench
                                                              "AckermannCps 2 3")
                                                            1000)
                                                          (scm:lambda
                                                            (v)
                                                            ((PureScheme.BenchmarkTest.AckermannCps.test
                                                                2)
                                                              3))))
                                                      (scm:lambda
                                                        ($__unused)
                                                        ((((Control.Bind.discard
                                                                Control.Bind.discardUnit)
                                                              Effect.bindEffect)
                                                            (((bench
                                                                  "AckermannCps 2 4")
                                                                1000)
                                                              (scm:lambda
                                                                (v)
                                                                ((PureScheme.BenchmarkTest.AckermannCps.test
                                                                    2)
                                                                  4))))
                                                          (scm:lambda
                                                            ($__unused)
                                                            ((((Control.Bind.discard
                                                                    Control.Bind.discardUnit)
                                                                  Effect.bindEffect)
                                                                (((bench
                                                                      "AckermannCps 3 1")
                                                                    1000)
                                                                  (scm:lambda
                                                                    (v)
                                                                    ((PureScheme.BenchmarkTest.AckermannCps.test
                                                                        3)
                                                                      1))))
                                                              (scm:lambda
                                                                ($__unused)
                                                                ((((Control.Bind.discard
                                                                        Control.Bind.discardUnit)
                                                                      Effect.bindEffect)
                                                                    (((bench
                                                                          "AckermannCps 3 2")
                                                                        1000)
                                                                      (scm:lambda
                                                                        (v)
                                                                        ((PureScheme.BenchmarkTest.AckermannCps.test
                                                                            3)
                                                                          2))))
                                                                  (scm:lambda
                                                                    ($__unused)
                                                                    ((((Control.Bind.discard
                                                                            Control.Bind.discardUnit)
                                                                          Effect.bindEffect)
                                                                        (((bench
                                                                              "RecordUpdate    1000")
                                                                            1000)
                                                                          (scm:lambda
                                                                            (v)
                                                                            (PureScheme.BenchmarkTest.RecordUpdate.test
                                                                              1000))))
                                                                      (scm:lambda
                                                                        ($__unused)
                                                                        ((((Control.Bind.discard
                                                                                Control.Bind.discardUnit)
                                                                              Effect.bindEffect)
                                                                            (((bench
                                                                                  "RecordUpdate   10000")
                                                                                1000)
                                                                              (scm:lambda
                                                                                (v)
                                                                                (PureScheme.BenchmarkTest.RecordUpdate.test
                                                                                  10000))))
                                                                          (scm:lambda
                                                                            ($__unused)
                                                                            ((((Control.Bind.discard
                                                                                    Control.Bind.discardUnit)
                                                                                  Effect.bindEffect)
                                                                                (((bench
                                                                                      "RecordUpdate  100000")
                                                                                    1000)
                                                                                  (scm:lambda
                                                                                    (v)
                                                                                    (PureScheme.BenchmarkTest.RecordUpdate.test
                                                                                      100000))))
                                                                              (scm:lambda
                                                                                ($__unused)
                                                                                ((((Control.Bind.discard
                                                                                        Control.Bind.discardUnit)
                                                                                      Effect.bindEffect)
                                                                                    (((bench
                                                                                          "RecordUpdate 1000000")
                                                                                        1000)
                                                                                      (scm:lambda
                                                                                        (v)
                                                                                        (PureScheme.BenchmarkTest.RecordUpdate.test
                                                                                          1000000))))
                                                                                  (scm:lambda
                                                                                    ($__unused)
                                                                                    ((((Control.Bind.discard
                                                                                            Control.Bind.discardUnit)
                                                                                          Effect.bindEffect)
                                                                                        (((bench
                                                                                              "ListInsert    1000")
                                                                                            1000)
                                                                                          (scm:lambda
                                                                                            (v)
                                                                                            (PureScheme.BenchmarkTest.ListInsert.test
                                                                                              1000))))
                                                                                      (scm:lambda
                                                                                        ($__unused)
                                                                                        ((((Control.Bind.discard
                                                                                                Control.Bind.discardUnit)
                                                                                              Effect.bindEffect)
                                                                                            (((bench
                                                                                                  "ListInsert   10000")
                                                                                                1000)
                                                                                              (scm:lambda
                                                                                                (v)
                                                                                                (PureScheme.BenchmarkTest.ListInsert.test
                                                                                                  10000))))
                                                                                          (scm:lambda
                                                                                            ($__unused)
                                                                                            ((((Control.Bind.discard
                                                                                                    Control.Bind.discardUnit)
                                                                                                  Effect.bindEffect)
                                                                                                (((bench
                                                                                                      "ListInsert  100000")
                                                                                                    1000)
                                                                                                  (scm:lambda
                                                                                                    (v)
                                                                                                    (PureScheme.BenchmarkTest.ListInsert.test
                                                                                                      100000))))
                                                                                              (scm:lambda
                                                                                                ($__unused)
                                                                                                (((bench
                                                                                                      "ListInsert 1000000")
                                                                                                    1000)
                                                                                                  (scm:lambda
                                                                                                    (v)
                                                                                                    (PureScheme.BenchmarkTest.ListInsert.test
                                                                                                      1000000))))))))))))))))))))))))))))))))))))))))))))))))))
  )