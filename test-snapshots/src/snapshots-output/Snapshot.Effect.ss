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
    (prefix (purescm runtime) rt:)
    (prefix (Data.Unit lib) Data.Unit.))

  (scm:define dontInlineMe
    (scm:lambda (v0)
      (scm:lambda ()
        Data.Unit.unit)))

  (scm:define lastComponentIsRun
    (scm:let ([_0 (dontInlineMe (rt:string->pstring "a"))])
      (scm:lambda ()
        (scm:let*
          ([_ (_0)]
           [_ ((dontInlineMe (rt:string->pstring "b")))])
            ((dontInlineMe (rt:string->pstring "c")))))))

  (scm:define lastPureIsUnwrapped
    (scm:let ([_0 (dontInlineMe (rt:string->pstring "a"))])
      (scm:lambda ()
        (scm:let*
          ([value1 (_0)]
           [_ ((dontInlineMe (rt:string->pstring "b")))])
            value1))))

  (scm:define main
    (scm:lambda ()
      (scm:let ([_ (lastComponentIsRun)])
        (lastPureIsUnwrapped)))))
