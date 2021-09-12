(library
  (PureScheme.Test.Recursion.Fibonacci lib)
  (export fib)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    fib
    (scm:lambda
      (v)
      (scm:cond
        ((scm:and (scm:= v 0)) 0)
        ((scm:and (scm:= v 1)) 1)
        ((scm:and #t) (scm:+ (fib (scm:- v 1)) (fib (scm:- v 2))))
        (scm:else (scm:error #f "Failed pattern match")))))
  )