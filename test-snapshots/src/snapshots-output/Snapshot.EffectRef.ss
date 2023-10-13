#!r6rs
#!chezscheme
(library
  (Snapshot.EffectRef lib)
  (export
    basicTest
    main
    onLet
    onLetTest
    positionZero
    primEffectAtTheEnd)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:)
    (prefix (Data.Unit lib) Data.Unit.)
    (prefix (Test.Assert lib) Test.Assert.))

  (scm:define primEffectAtTheEnd
    (scm:lambda ()
      (scm:let ([n0 (scm:box 1)])
        (scm:unbox n0))))

  (scm:define positionZero
    (scm:lambda ()
      (scm:box 0)))

  (scm:define onLet
    (scm:lambda (x0)
      (scm:let*
        ([a1 (scm:fx+ x0 x0)]
         [_2 (scm:fx+ (scm:fx+ a1 a1) x0)])
          (scm:lambda ()
            (scm:box _2)))))

  (scm:define onLetTest
    (scm:let ([_0 (onLet 1)])
      (scm:lambda ()
        (scm:let*
          ([n1 (_0)]
           [v2 (scm:unbox n1)])
            ((Test.Assert.assert (scm:fx=? v2 5)))))))

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
      (scm:let*
        ([_ (basicTest)]
         [_ (onLetTest)]
         [_ (primEffectAtTheEnd)])
          Data.Unit.unit))))
