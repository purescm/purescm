#!r6rs
#!chezscheme
(library
  (Snapshot.Effect lib)
  (export
    dontInlineMe
    lastComponentIsRun
    lastPureIsUnwrapped
    main)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Data.Unit lib) Data.Unit.))

  (scm:define dontInlineMe
    (scm:lambda (v0)
      (scm:lambda ()
        Data.Unit.unit)))

  (scm:define lastComponentIsRun
    (scm:let ([_0 (dontInlineMe "a")])
      (scm:lambda ()
        (scm:let*
          ([_ (_0)]
          [_ ((dontInlineMe "b"))])
            ((dontInlineMe "c"))))))

  (scm:define lastPureIsUnwrapped
    (scm:let ([_0 (dontInlineMe "a")])
      (scm:lambda ()
        (scm:let*
          ([value1 (_0)]
          [_ ((dontInlineMe "b"))])
            value1))))

  (scm:define main
    (scm:lambda ()
      (scm:let ([_ (lastComponentIsRun)])
        (lastPureIsUnwrapped)))))
