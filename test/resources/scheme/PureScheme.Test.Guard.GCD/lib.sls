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
          ((scm:= v1 0) v)
          ((scm:= v 0) v1)
          (#t
            (scm:cond
              ((((Data.Ord.greaterThan Data.Ord.ordInt) v) v1)
                ((gcd (scm:- v v1)) v1))
              (Data.Boolean.otherwise ((gcd v) (scm:- v1 v)))
              (scm:else (scm:error #f "Failed pattern match"))))
          (scm:else (scm:error #f "Failed pattern match"))))))
  )