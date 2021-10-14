(library
  (PureScheme.Test.Binder.Guard lib)
  (export match)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Boolean lib) Data.Boolean.)
    (prefix (Data.Eq lib) Data.Eq.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    match
    (scm:lambda
      (dictPartial)
      (scm:lambda
        (v)
        (scm:lambda
          (a)
          (scm:cond
            ((scm:= v 1)
              (scm:cond
                ((((Data.Eq.eq Data.Eq.eqInt) a) 2) 3)
                ((((Data.Eq.eq Data.Eq.eqInt) a) 4) 5)
                (Data.Boolean.otherwise 6)
                (scm:else
                  (scm:error
                    #f
                    "Failed pattern match ZANQKUTQILKFDFILZRORTXQI"))))
            (scm:else
              (scm:error
                #f
                "Failed pattern match RTELPWOCHHGOQKMAKNFBEPAJ")))))))
  )