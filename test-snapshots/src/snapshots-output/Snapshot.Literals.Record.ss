#!r6rs
#!chezscheme
(library
  (Snapshot.Literals.Record lib)
  (export
    main
    recordAccess
    recordUpdate)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Test.Assert lib) Test.Assert.))

  (scm:define recordUpdate
    (scm:lambda (v0)
      (scm:let ([$record (scm:hashtable-copy v0)])
        ((scm:hashtable-set! $record "fooBarBaz" 10)))))

  (scm:define recordAccess
    (scm:lambda (v0)
      (scm:hashtable-ref v0 "fooBarBaz" #f)))

  (scm:define main
    (scm:let*
      ([r0 (scm:letrec* (($record (scm:make-hashtable scm:string-hash scm:string=?))) (scm:hashtable-set! $record "fooBarBaz" 5) $record)]
       [_1 (Test.Assert.assert (scm:fx=? (recordAccess (recordUpdate r0)) 10))])
        (scm:lambda ()
          (scm:let ([_ (_1)])
            ((Test.Assert.assert (scm:fx=? (recordAccess r0) 5))))))))
