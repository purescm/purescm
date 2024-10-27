#!r6rs
#!chezscheme
(library
  (Snapshot.PrimUndefined lib)
  (export
    main
    testCase)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Test.Assert lib) Test.Assert.))

  (scm:define testCase
    (scm:lambda (dictRing0)
      (rt:record-ref ((rt:record-ref dictRing0 (scm:string->symbol "Semiring0")) (scm:quote undefined)) (scm:string->symbol "add"))))

  (scm:define main
    (Test.Assert.assert (scm:fx=? (((testCase Data.Ring.ringInt) 1) 1) 2))))
