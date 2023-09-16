#!r6rs
#!chezscheme
(library
  (Snapshot.Literals.Record lib)
  (export
    insert
    main
    minusTwo
    recordAccess
    recordAddField
    recordUpdate)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:)
    (prefix (Record lib) Record.)
    (prefix (Test.Assert lib) Test.Assert.)
    (prefix (Type.Proxy lib) Type.Proxy.)
    (Snapshot.Literals.Record foreign))

  (scm:define insert
    (((Record.insert (rt:make-object (scm:cons "reflectSymbol" (scm:lambda (_)
      "anotherField")))) (scm:gensym "purs-undefined")) (scm:gensym "purs-undefined")))

  (scm:define recordUpdate
    (scm:lambda (v0)
      (scm:let ([$record (rt:object-copy v0)])
        (scm:begin (rt:object-set! $record "fooBarBaz" 10)))))

  (scm:define recordAddField
    (scm:lambda (_)
      ((insert Type.Proxy.Proxy) 42)))

  (scm:define recordAccess
    (scm:lambda (v0)
      (rt:object-ref v0 "fooBarBaz")))

  (scm:define main
    (scm:let*
      ([r0 (rt:make-object (scm:cons "fooBarBaz" 5))]
       [s1 (recordUpdate r0)]
       [t2 ((recordAddField (scm:gensym "purs-undefined")) s1)]
       [_3 (Test.Assert.assert (scm:fx=? (recordAccess t2) 10))])
        (scm:lambda ()
          (scm:let*
            ([_ (_3)]
             [_ ((Test.Assert.assert (scm:fx=? (rt:object-ref t2 "anotherField") 42)))]
             [_ ((Test.Assert.assert (scm:fx=? (recordAccess s1) 10)))]
             [_ ((Test.Assert.assert (scm:fx=? (recordAccess r0) 5)))])
              ((Test.Assert.assert (scm:fx=? (recordAccess (rt:make-object (scm:cons "fooBarBaz" minusTwo))) minusTwo))))))))
