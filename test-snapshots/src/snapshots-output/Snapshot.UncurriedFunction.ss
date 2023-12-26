#!r6rs
#!chezscheme
(library
  (Snapshot.UncurriedFunction lib)
  (export
    main
    test1a
    test1b
    test2a
    test2b
    test3a
    test3b
    test4a
    test4b)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Data.Function.Uncurried lib) Data.Function.Uncurried.)
    (prefix (Effect.Console lib) Effect.Console.)
    (prefix (Test.Assert lib) Test.Assert.))

  (scm:define test4a
    (scm:lambda (a0)
      (scm:let*
        ([_1 (Effect.Console.log a0)]
         [_ (_1)])
          a0)))

  (scm:define test4b
    (scm:lambda ()
      (test4a (rt:string->bytestring "test4b"))))

  (scm:define test3a
    (scm:lambda (v0 b1)
      b1))

  (scm:define test3b
    (test3a 1 2))

  (scm:define test2a
    (scm:lambda (a0 v1)
      a0))

  (scm:define test2b
    (test2a 1 2))

  (scm:define test1a
    (Data.Function.Uncurried.mkFn0 (scm:lambda (v0)
      1)))

  (scm:define test1b
    (Data.Function.Uncurried.runFn0 test1a))

  (scm:define main
    (scm:let ([_0 (Test.Assert.assert (scm:fx=? test1b 1))])
      (scm:lambda ()
        (scm:let*
          ([_ (_0)]
           [_ ((Test.Assert.assert (scm:fx=? test2b 1)))]
           [_ ((Test.Assert.assert (scm:fx=? test3b 2)))]
           [v4 (test4b)]
           [_ ((Test.Assert.assert (rt:bytestring=? v4 (rt:string->bytestring "test4b"))))]
           [w6 (test4a (rt:string->bytestring "test4b"))])
            ((Test.Assert.assert (rt:bytestring=? w6 (rt:string->bytestring "test4b")))))))))
