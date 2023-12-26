#!r6rs
#!chezscheme
(library
  (Snapshot.List lib)
  (export
    car
    cdr
    cons2
    curried
    main
    show
    xs)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Data.List.Types lib) Data.List.Types.)
    (prefix (Data.Maybe lib) Data.Maybe.)
    (prefix (Data.Show lib) Data.Show.)
    (prefix (Test.Assert lib) Test.Assert.))

  (scm:define show
    (rt:object-ref (Data.List.Types.showList Data.Show.showInt) (scm:string->symbol "show")))

  (scm:define xs
    (scm:cons 1 (scm:quote ())))

  (scm:define curried
    rt:list-cons)

  (scm:define cons2
    (rt:list-cons 2))

  (scm:define cdr
    (scm:lambda (v0)
      (scm:cond
        [(scm:null? v0) Data.Maybe.Nothing]
        [(scm:pair? v0) (Data.Maybe.Just (scm:cdr v0))]
        [scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match")))])))

  (scm:define car
    (scm:lambda (v0)
      (scm:cond
        [(scm:null? v0) Data.Maybe.Nothing]
        [(scm:pair? v0) (Data.Maybe.Just (scm:car v0))]
        [scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match")))])))

  (scm:define main
    (scm:let ([_0 (Test.Assert.assert #t)])
      (scm:lambda ()
        (scm:let*
          ([_ (_0)]
           [_ ((Test.Assert.assert (scm:letrec ([go2 (scm:lambda (v3)
            (scm:lambda (v14)
              (scm:lambda (v25)
                (scm:cond
                  [(scm:not v25) #f]
                  [(scm:null? v3) (scm:and (scm:null? v14) v25)]
                  [scm:else (scm:and (scm:pair? v3) (scm:and (scm:pair? v14) (((go2 (scm:cdr v3)) (scm:cdr v14)) (scm:and v25 (scm:fx=? (scm:car v14) (scm:car v3))))))]))))])
            (((go2 (scm:cons 2 (scm:quote ()))) (scm:cons 2 (scm:quote ()))) #t))))]
           [_ ((Test.Assert.assert (scm:letrec ([go3 (scm:lambda (v4)
            (scm:lambda (v15)
              (scm:lambda (v26)
                (scm:cond
                  [(scm:not v26) #f]
                  [(scm:null? v4) (scm:and (scm:null? v15) v26)]
                  [scm:else (scm:and (scm:pair? v4) (scm:and (scm:pair? v15) (((go3 (scm:cdr v4)) (scm:cdr v15)) (scm:and v26 (scm:fx=? (scm:car v15) (scm:car v4))))))]))))])
            (((go3 (scm:cons 1 (scm:quote ()))) (scm:cons 1 (scm:quote ()))) #t))))]
           [_ ((Test.Assert.assert (scm:letrec ([go4 (scm:lambda (v5)
            (scm:lambda (v16)
              (scm:lambda (v27)
                (scm:cond
                  [(scm:not v27) #f]
                  [(scm:null? v5) (scm:and (scm:null? v16) v27)]
                  [scm:else (scm:and (scm:pair? v5) (scm:and (scm:pair? v16) (((go4 (scm:cdr v5)) (scm:cdr v16)) (scm:and v27 (scm:fx=? (scm:car v16) (scm:car v5))))))]))))])
            (((go4 (cons2 (scm:cons 3 (scm:quote ())))) (scm:cons 2 (scm:cons 3 (scm:quote ())))) #t))))])
            ((Test.Assert.assert (rt:bytestring=? (show (scm:cons 1 (scm:cons 2 (scm:quote ())))) (rt:string->bytestring "(1 : 2 : Nil)")))))))))
