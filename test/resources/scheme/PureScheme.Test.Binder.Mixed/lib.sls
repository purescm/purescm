(library
  (PureScheme.Test.Binder.Mixed lib)
  (export foo)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    foo
    (scm:lambda
      (v)
      (scm:lambda
        (v1)
        (scm:cond
          ((scm:and (scm:= v 0) (scm:= v1 0)) 0)
          ((scm:and (scm:= v 0) (scm:= v1 1)) 1)
          ((scm:and (scm:= v 1) (scm:= v1 0)) 10)
          ((scm:and #t (scm:= v1 2)) 2)
          ((scm:and (scm:= v 3) #t) 30)
          ((scm:and (scm:= v 4) #t) v1)
          ((scm:and #t (scm:= v1 5)) v)
          ((scm:and #t #t) (scm:+ v v1))
          (scm:else (scm:error #f "Failed pattern match"))))))
  )