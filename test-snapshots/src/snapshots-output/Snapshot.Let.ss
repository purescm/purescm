#!r6rs
#!chezscheme
(library
  (Snapshot.Let lib)
  (export
    isEven
    isOdd
    letChain
    letRecursive)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:))

  (scm:define letRecursive
    (scm:lambda (x0)
      (scm:letrec ([go1 (scm:lambda (v2)
        (scm:cond
          [(scm:fx=? v2 0) 0]
          [scm:else (go1 (scm:fx- v2 1))]))])
        (go1 x0))))

  (scm:define letChain
    (scm:lambda (x0)
      (scm:let*
        ([a1 (scm:fx+ x0 x0)]
         [b2 (scm:fx+ a1 a1)]
         [c3 (scm:fx+ b2 b2)])
          (scm:fx+ (scm:fx+ (scm:fx+ a1 b2) c3) (scm:fx* c3 c3)))))

  (scm:define isOdd
    (scm:lambda (x0)
      (scm:cond
        [(scm:fx=? x0 1) #f]
        [scm:else (isEven (scm:fx- x0 1))])))

  (scm:define isEven
    (scm:lambda (x0)
      (scm:cond
        [(scm:fx=? x0 0) #t]
        [scm:else (scm:let ([_1 (scm:fx- x0 1)])
          (scm:cond
            [(scm:fx=? _1 1) #f]
            [scm:else (isEven (scm:fx- _1 1))]))]))))
