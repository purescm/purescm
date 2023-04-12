#!r6rs
#!chezscheme
(library
  (Snapshot.Let lib)
  (export
    isEven
    isOdd
    letChain
    letChainMix
    letChainRecursive
    letRecursive)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define letRecursive
    (scm:lambda (x0)
      (scm:cond
        [(scm:fx=? x0 0) 0]
        [scm:else (letRecursive (scm:fx- x0 1))])))

  (scm:define letChainRecursive
    (scm:lambda (n0)
      (scm:letrec*
        ([isOdd$p1 (scm:lambda (x2)
          (scm:cond
            [(scm:fx=? x2 0) #f]
            [scm:else (isEven$p1 (scm:fx- x2 1))]))]
         [isEven$p1 (scm:lambda (x2)
          (scm:cond
            [(scm:fx=? x2 0) #t]
            [scm:else (isOdd$p1 (scm:fx- x2 1))]))])
          (scm:vector (isEven$p1 n0) (isOdd$p1 n0)))))

  (scm:define letChainMix
    (scm:lambda (n0)
      (scm:let*
        ([o1 (scm:fx* n0 2)]
         [p2 (scm:fx* o1 2)])
          (scm:letrec*
            ([isOdd$p3 (scm:lambda (x4)
              (scm:cond
                [(scm:fx=? x4 0) #f]
                [scm:else (isEven$p3 (scm:fx- x4 1))]))]
             [isEven$p3 (scm:lambda (x4)
              (scm:cond
                [(scm:fx=? x4 0) #t]
                [scm:else (isOdd$p3 (scm:fx- x4 1))]))])
              (scm:vector (isEven$p3 n0) (isOdd$p3 n0) (isEven$p3 o1) (isOdd$p3 o1) (isEven$p3 p2) (isOdd$p3 p2))))))

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
        [(scm:fx=? x0 0) #f]
        [scm:else (isEven (scm:fx- x0 1))])))

  (scm:define isEven
    (scm:lambda (x0)
      (scm:cond
        [(scm:fx=? x0 0) #t]
        [scm:else (scm:let ([_1 (scm:fx- x0 1)])
          (scm:cond
            [(scm:fx=? _1 0) #f]
            [scm:else (isEven (scm:fx- _1 1))]))]))))
