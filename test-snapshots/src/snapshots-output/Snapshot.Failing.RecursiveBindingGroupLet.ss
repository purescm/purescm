#!r6rs
#!chezscheme
(library
  (Snapshot.Failing.RecursiveBindingGroupLet lib)
  (export
    main
    test)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Data.Unit lib) Data.Unit.))

  (scm:define test
    (scm:letrec*
      ([test30 (scm:lambda (n1)
        (scm:cond
          [(scm:fx<? n1 100) n1]
          [scm:else (rt:record-ref test10 (scm:string->symbol "bar"))]))]
       [test20 (scm:list (scm:cons (scm:string->symbol "baz") (rt:record-ref test10 (scm:string->symbol "bar"))))]
       [test10 (scm:list (scm:cons (scm:string->symbol "foo") (rt:record-ref test20 (scm:string->symbol "baz"))) (scm:cons (scm:string->symbol "bar") (test30 42)))])
        (rt:record-ref test10 (scm:string->symbol "bar"))))

  (scm:define main
    (scm:lambda ()
      Data.Unit.unit)))
