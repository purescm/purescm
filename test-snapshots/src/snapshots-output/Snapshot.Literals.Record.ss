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
    recordUpdate
    unsafeGetNotFound)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Record lib) Record.)
    (prefix (Test.Assert lib) Test.Assert.)
    (prefix (Type.Proxy lib) Type.Proxy.)
    (Snapshot.Literals.Record foreign))

  (scm:define insert
    (((Record.insert (rt:make-object (scm:cons (scm:string->symbol "reflectSymbol") (scm:lambda (_)
      (rt:string->bytestring "anotherField"))))) (scm:gensym "purs-undefined")) (scm:gensym "purs-undefined")))

  (scm:define recordUpdate
    (scm:lambda (v0)
      (scm:let ([$record (rt:object-copy v0)])
        (scm:begin (rt:object-set! $record (scm:string->symbol "fooBarBaz") 10) $record))))

  (scm:define recordAddField
    (scm:lambda (_)
      ((insert Type.Proxy.Proxy) 42)))

  (scm:define recordAccess
    (scm:lambda (v0)
      (rt:object-ref v0 (scm:string->symbol "fooBarBaz"))))

  (scm:define main
    (scm:let*
      ([r0 (rt:make-object (scm:cons (scm:string->symbol "fooBarBaz") 5))]
       [s1 (recordUpdate r0)]
       [t2 ((recordAddField (scm:gensym "purs-undefined")) s1)]
       [u3 (rt:make-object (scm:cons (scm:string->symbol "fooBarBaz") minusTwo))]
       [_4 (Test.Assert.assert (scm:fx=? (recordAccess t2) 10))])
        (scm:lambda ()
          (scm:let*
            ([_ (_4)]
             [_ ((Test.Assert.assert (scm:fx=? (rt:object-ref t2 (scm:string->symbol "anotherField")) 42)))]
             [_ ((Test.Assert.assert (scm:fx=? (recordAccess s1) 10)))]
             [_ ((Test.Assert.assert (scm:fx=? (recordAccess r0) 5)))]
             [_ ((Test.Assert.assert (scm:fx=? (recordAccess u3) minusTwo)))])
              (((Test.Assert.assertThrows$p (rt:string->bytestring "Assertion failed: An error should have been thrown")) (scm:lambda (v10)
                (unsafeGetNotFound u3)))))))))
