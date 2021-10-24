(library
  (PureScheme.BenchmarkTest.FibonacciTco lib)
  (export test)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    test
    (scm:lambda
      (n)
      (scm:letrec*
        ((fib
          (scm:lambda
            (v)
            (scm:lambda
              (a)
              (scm:lambda
                (b)
                (scm:cond
                  ((scm:= v 0) a)
                  (#t (((fib (scm:fx- v 1)) b) (scm:fx+ a b)))
                  (scm:else (scm:error #f "Failed pattern match"))))))))
        (((fib n) 0) 1))))
  )