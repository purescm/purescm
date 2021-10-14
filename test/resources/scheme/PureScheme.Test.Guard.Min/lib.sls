(library
  (PureScheme.Test.Guard.Min lib)
  (export min)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Boolean lib) Data.Boolean.)
    (prefix (Data.Ord lib) Data.Ord.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    min
    (scm:lambda
      (n)
      (scm:lambda
        (m)
        (scm:cond
          (#t
            (scm:cond
              ((((Data.Ord.lessThan Data.Ord.ordInt) n) m) n)
              (Data.Boolean.otherwise m)
              (scm:else
                (scm:error
                  #f
                  "Failed pattern match NLWWVIBZJNCWDULPATLTCSXV"))))
          (scm:else
            (scm:error #f "Failed pattern match TBVPQFWDBPJKTXPEPHSVFINO"))))))
  )