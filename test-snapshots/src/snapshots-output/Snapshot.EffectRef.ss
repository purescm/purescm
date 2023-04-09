#!r6rs
#!chezscheme
(library
  (Snapshot.EffectRef lib)
  (export
    basicTest
    main
    onLet
    onLetTest
    positionZero)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Test.Assert lib) Test.Assert.))

  (scm:define positionZero
    (scm:lambda ()
      (scm:box 0)))

  (scm:define onLet
    (scm:lambda (x0)
      (scm:let*
        ([a1 (scm:fx+ x0 x0)]
         [_2 (scm:fx+ a1 (scm:fx+ a1 x0))])
          (scm:lambda ()
            (scm:box _2)))))

  (scm:define onLetTest
    (scm:lambda ()
      (scm:let*
        ([n0 (scm:box 5)]
         [v1 (scm:unbox n0)])
          ((Test.Assert.assert (scm:fx=? v1 5))))))

  (scm:define basicTest
    (scm:lambda ()
      (scm:let*
        ([n0 (scm:box 0)]
         [_1 (scm:unbox n0)]
         [a$p2 (scm:set-box! n0 (scm:fx+ _1 1))]
         [v3 (scm:unbox n0)])
          ((Test.Assert.assert (scm:fx=? v3 1))))))

  (scm:define main
    (scm:lambda ()
      (scm:let ([_ (basicTest)])
        (onLetTest)))))
