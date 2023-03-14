(library
  (PureScheme.BenchmarkTest.ListInsert lib)
  (export Nil Cons test)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ord lib) Data.Ord.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define Nil (scm:cons (scm:quote Nil) (scm:vector)))

  (scm:define
    Cons
    (scm:lambda
      (value0)
      (scm:lambda
        (value1)
        (scm:cons (scm:quote Cons) (scm:vector value0 value1)))))

  (scm:define
    test
    (scm:lambda
      (n)
      (scm:letrec*
        ((go
          (scm:lambda
            (acc)
            (scm:lambda
              (x)
              (scm:cond
                ((scm:eq? (scm:fx<? x n) #t)
                  ((go ((Cons x) acc)) (scm:fx+ x 1)))
                (#t acc)
                (scm:else (scm:error #f "Failed pattern match")))))))
        ((go Nil) 0))))
  )