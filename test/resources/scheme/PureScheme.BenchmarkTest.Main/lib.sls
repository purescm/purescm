(library
  (PureScheme.BenchmarkTest.Main lib)
  (export fibonacci main)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Performance.Minibench lib) Performance.Minibench.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    fibonacci
    (scm:lambda
      (v)
      (scm:cond
        ((scm:= v 0) 0)
        ((scm:= v 1) 1)
        (#t (scm:fx+ (fibonacci (scm:fx- v 1)) (fibonacci (scm:fx- v 2))))
        (scm:else
          (scm:error #f "Failed pattern match FWXCDYYIONKOCCOXIHENHZWG")))))

  (scm:define main (Performance.Minibench.bench (scm:lambda (v) (fibonacci 5))))
  )