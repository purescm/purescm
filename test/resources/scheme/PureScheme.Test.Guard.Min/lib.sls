(library
  (PureScheme.Test.Guard.Min lib)
  (export min)
  (import
    (rnrs)
    (prefix (Data.Boolean lib) Data.Boolean.)
    (prefix (Data.Ord lib) Data.Ord.)
    (prefix (Prelude lib) Prelude.))


  (define
    min
    (lambda
      (n)
      (lambda
        (m)
        (cond
          (#t
            (cond
              ((((Data.Ord.lessThan Data.Ord.ordInt) n) m) n)
              (Data.Boolean.otherwise m)
              (else (error #f "Failed pattern match"))))
          (else (error #f "Failed pattern match"))))))
  )