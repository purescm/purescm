(library
  (PureScheme.Test.Let.LambdasInFunction lib)
  (export foo)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    foo
    (scm:lambda
      (dictRing)
      (scm:lambda
        (x)
        (scm:lambda
          (y)
          (scm:letrec*
            ((sub
                (scm:lambda
                  (a)
                  (scm:lambda (b) (((Data.Ring.sub dictRing) x) y))))
              (add
                (scm:lambda
                  (a)
                  (scm:lambda
                    (b)
                    (((Data.Semiring.add
                          ((scm:hashtable-ref
                              dictRing
                              "Semiring0"
                              (scm:error #f "Key not found"))
                            (scm:error #f "undefined")))
                        x)
                      y)))))
            ((add ((sub x) y)) ((add x) y)))))))
  )