(library
  (PureScheme.BenchmarkTest.Ackermann lib)
  (export test)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Function lib) Data.Function.)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    test
    (scm:lambda
      (v)
      (scm:lambda
        (v1)
        (scm:cond
          ((scm:= v 0) (scm:fx+ v1 1))
          ((scm:= v1 0) ((test (scm:fx- v 1)) 1))
          (#t
            ((Data.Function.apply (test (scm:fx- v 1)))
              ((test v) (scm:fx- v1 1))))
          (scm:else (scm:error #f "Failed pattern match"))))))
  )