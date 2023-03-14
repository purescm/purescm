(library
  (PureScheme.BenchmarkTest.Fibonacci lib)
  (export test)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    test
    (scm:lambda
      (v)
      (scm:cond
        ((scm:= v 0) 0)
        ((scm:= v 1) 1)
        (#t (scm:fx+ (test (scm:fx- v 1)) (test (scm:fx- v 2))))
        (scm:else (scm:error #f "Failed pattern match")))))
  )