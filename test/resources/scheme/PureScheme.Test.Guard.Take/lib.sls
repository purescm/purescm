(library
  (PureScheme.Test.Guard.Take lib)
  (export Nil Cons take)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ord lib) Data.Ord.)
    (prefix (Data.Ring lib) Data.Ring.)
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
    take
    (scm:letrec*
      ((go
        (scm:lambda
          (acc)
          (scm:lambda
            (v)
            (scm:lambda
              (v1)
              (scm:cond
                ((scm:fx<? v 1) acc)
                ((scm:eq? (scm:car v1) (scm:quote Nil)) acc)
                ((scm:eq? (scm:car v1) (scm:quote Cons))
                  (((go ((Cons (scm:vector-ref (scm:cdr v1) 0)) acc))
                      (scm:fx- v 1))
                    (scm:vector-ref (scm:cdr v1) 1)))
                (scm:else (scm:error #f "Failed pattern match"))))))))
      (go Nil)))
  )