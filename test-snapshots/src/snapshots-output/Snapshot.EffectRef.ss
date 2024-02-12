#!r6rs
#!chezscheme
(library
  (Snapshot.EffectRef lib)
  (export
    basicTest
    main
    newCase
    onLet
    onLetTest
    positionZero
    primEffectAtTheEnd)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Data.Show lib) Data.Show.)
    (prefix (Data.Unit lib) Data.Unit.)
    (prefix (Effect.Console lib) Effect.Console.)
    (prefix (Effect.Ref lib) Effect.Ref.)
    (prefix (Test.Assert lib) Test.Assert.))

  (scm:define primEffectAtTheEnd
    (scm:let ([_0 (Effect.Ref._new 1)])
      (scm:lambda ()
        (scm:let ([n1 (_0)])
          ((Effect.Ref.read n1))))))

  (scm:define positionZero
    (Effect.Ref._new 0))

  (scm:define onLet
    (scm:lambda (x0)
      (scm:let ([a1 (scm:fx+ x0 x0)])
        (Effect.Ref._new (scm:fx+ (scm:fx+ a1 a1) x0)))))

  (scm:define onLetTest
    (scm:let ([_0 (onLet 1)])
      (scm:lambda ()
        (scm:let*
          ([n1 (_0)]
           [v2 ((Effect.Ref.read n1))])
            ((Test.Assert.assert (scm:fx=? v2 5)))))))

  (scm:define newCase
    (scm:let ([_0 (Effect.Ref._new 0)])
      (scm:lambda ()
        (scm:let*
          ([counter1 (_0)]
           [newCounter2 (((Effect.Ref.modifyImpl (scm:lambda (s2)
            (scm:let ([s$p3 (scm:fx+ s2 1)])
              (scm:list (scm:cons (scm:string->symbol "state") s$p3) (scm:cons (scm:string->symbol "value") s$p3))))) counter1))]
           [_ ((Effect.Console.log (scm:string-append "New counter is " (Data.Show.showIntImpl newCounter2))))])
            ((Test.Assert.assert (scm:fx=? newCounter2 1)))))))

  (scm:define basicTest
    (scm:let ([_0 (Effect.Ref._new 0)])
      (scm:lambda ()
        (scm:let*
          ([n1 (_0)]
           [_ (((Effect.Ref.modify_ (scm:lambda (v2)
            (scm:fx+ v2 1))) n1))]
           [v3 ((Effect.Ref.read n1))])
            ((Test.Assert.assert (scm:fx=? v3 1)))))))

  (scm:define main
    (scm:lambda ()
      (scm:let*
        ([_ (basicTest)]
         [_ (onLetTest)]
         [_ (newCase)]
         [_ (primEffectAtTheEnd)])
          Data.Unit.unit))))
