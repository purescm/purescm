#!r6rs
#!chezscheme
(library
  (Snapshot.Failing.RecursiveBindingGroupTopLevel lib)
  (export
    main
    test1
    test2
    test3)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Data.Unit lib) Data.Unit.))

  (scm:define test3
    (scm:lambda (n0)
      (scm:cond
        [(scm:fx<? n0 100) n0]
        [scm:else (rt:record-ref ($lazy-test1) (scm:string->symbol "bar"))])))

  (rt:define-lazy "test2" "Snapshot.Failing.RecursiveBindingGroupTopLevel" $lazy-test2
    (scm:list (scm:cons (scm:string->symbol "baz") (rt:record-ref ($lazy-test1) (scm:string->symbol "bar")))))

  (rt:define-lazy "test1" "Snapshot.Failing.RecursiveBindingGroupTopLevel" $lazy-test1
    (scm:list (scm:cons (scm:string->symbol "foo") (rt:record-ref ($lazy-test1) (scm:string->symbol "bar"))) (scm:cons (scm:string->symbol "bar") 42)))

  (scm:define test2
    ($lazy-test2))

  (scm:define test1
    ($lazy-test1))

  (scm:define main
    (scm:lambda ()
      Data.Unit.unit)))
