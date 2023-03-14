(library
  (PureScheme.BenchmarkTest.AckermannCps lib)
  (export test)
  (import
    (prefix (rnrs) scm:)
    (prefix (Control.Category lib) Control.Category.)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    test
    (scm:lambda
      (x)
      (scm:lambda
        (y)
        (scm:letrec*
          ((go
            (scm:lambda
              (v)
              (scm:lambda
                (v1)
                (scm:lambda
                  (cont)
                  (scm:cond
                    ((scm:= v 0) (cont (scm:fx+ v1 1)))
                    ((scm:= v1 0) (((go (scm:fx- v 1)) 1) cont))
                    (#t
                      (((go v) (scm:fx- v1 1))
                        (scm:lambda (a) (((go (scm:fx- v 1)) a) cont))))
                    (scm:else (scm:error #f "Failed pattern match"))))))))
          (((go x) y)
            (Control.Category.identity Control.Category.categoryFn))))))
  )