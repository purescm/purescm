#!r6rs
#!chezscheme
(library
  (Snapshot.UncurriedFunction lib)
  (export
    test1a
    test1b
    test2a
    test2b
    test3a
    test3b)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Data.Function.Uncurried lib) Data.Function.Uncurried.))

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

  (scm:define test1b
    Data.Function.Uncurried.runFn0)

  (scm:define test1a
    (Data.Function.Uncurried.mkFn0 (scm:lambda (v0) 1))))
