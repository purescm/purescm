#!r6rs
#!chezscheme
(library
  (Snapshot.Literals.Record lib)
  (export
    insert
    main
    recordAccess
    recordAddField
    recordUpdate)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Record lib) Record.)
    (prefix (Test.Assert lib) Test.Assert.)
    (prefix (Type.Proxy lib) Type.Proxy.))

  (scm:define insert
    (((Record.insert (scm:letrec* (($record (scm:make-hashtable scm:string-hash scm:string=?))) (scm:hashtable-set! $record "reflectSymbol" (scm:lambda (_)
      "anotherField")) $record)) (scm:gensym "purs-undefined")) (scm:gensym "purs-undefined")))

  (scm:define recordUpdate
    (scm:lambda (v0)
      (scm:let ([$record (scm:hashtable-copy v0 #t)])
        ((scm:hashtable-set! $record "fooBarBaz" 10)))))

  (scm:define recordAddField
    (scm:lambda (_)
      ((insert Type.Proxy.Proxy) 42)))

  (scm:define recordAccess
    (scm:lambda (v0)
      (scm:hashtable-ref v0 "fooBarBaz" #f)))

  (scm:define main
    (scm:let*
      ([r0 (scm:letrec* (($record (scm:make-hashtable scm:string-hash scm:string=?))) (scm:hashtable-set! $record "fooBarBaz" 5) $record)]
       [s1 (recordUpdate r0)]
       [t2 (scm:let ([$record (scm:hashtable-copy s1 #t)])
        ((scm:hashtable-set! $record "anotherField" 42)))]
       [_3 (Test.Assert.assert (scm:fx=? (recordAccess t2) 10))])
        (scm:lambda ()
          (scm:let*
            ([_ (_3)]
             [_ ((Test.Assert.assert (scm:fx=? (scm:hashtable-ref t2 "anotherField" #f) 42)))]
             [_ ((Test.Assert.assert (scm:fx=? (recordAccess s1) 10)))])
              ((Test.Assert.assert (scm:fx=? (recordAccess r0) 5))))))))
