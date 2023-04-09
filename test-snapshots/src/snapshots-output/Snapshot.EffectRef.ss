#!r6rs
#!chezscheme
(library
  (Snapshot.EffectRef lib)
  (export
    example
    main)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Test.Assert lib) Test.Assert.))

  (scm:define main
    (scm:lambda ()
      (scm:let*
        ([n0 ((scm:lambda ()
          (scm:box 0)))]
         [_1 ((scm:lambda ()
          (scm:unbox n0)))]
         [a$p2 ((scm:lambda ()
          (scm:set-box! n0 (scm:fx+ _1 1))))]
         [v3 ((scm:lambda ()
          (scm:unbox n0)))])
          ((Test.Assert.assert (scm:fx=? v3 1))))))

  (scm:define example
    (scm:lambda ()
      (scm:box 0))))
