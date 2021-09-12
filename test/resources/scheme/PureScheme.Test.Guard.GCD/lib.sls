(library
  (PureScheme.Test.Guard.GCD lib)
  (export gcd)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Boolean lib) Data.Boolean.)
    (prefix (Data.Ord lib) Data.Ord.)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    gcd
    (scm:lambda
      (v)
      (scm:lambda
        (v1)
        (scm:cond
          ((scm:and #t (scm:= v1 0)) v)
          ((scm:and (scm:= v 0) #t) v1)
          ((scm:and #t #t)
            (scm:cond
              ((((Data.Ord.greaterThan Data.Ord.ordInt) v) v1)
                ((gcd (- v v1)) v1))
              (Data.Boolean.otherwise ((gcd v) (- v1 v)))
              (scm:else (scm:error #f "Failed pattern match"))))
          (scm:else (scm:error #f "Failed pattern match"))))))
  )