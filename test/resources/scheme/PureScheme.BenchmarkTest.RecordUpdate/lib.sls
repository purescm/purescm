(library
  (PureScheme.BenchmarkTest.RecordUpdate lib)
  (export test)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ord lib) Data.Ord.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    test
    (scm:lambda
      (n)
      (scm:letrec*
        ((go
          (scm:lambda
            (acc)
            (scm:lambda
              (x)
              (scm:cond
                ((scm:eq? (scm:fx<? x n) #t)
                  ((go
                      (scm:letrec*
                        ((v acc))
                        (scm:letrec*
                          (($ht
                            (scm:make-hashtable
                              scm:string-hash
                              scm:string=?
                              1)))
                          (scm:begin (scm:hashtable-set! $ht "a" x) $ht))))
                    (scm:fx+ x 1)))
                (#t acc)
                (scm:else (scm:error #f "Failed pattern match")))))))
        ((go
            (scm:letrec*
              (($ht (scm:make-hashtable scm:string-hash scm:string=? 1)))
              (scm:begin (scm:hashtable-set! $ht "a" 0) $ht)))
          1))))
  )