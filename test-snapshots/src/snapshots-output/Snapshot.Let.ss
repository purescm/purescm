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
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define letRecursive
    (scm:lambda (x0)
      (scm:if (scm:fx=? x0 0)
              0
              (letRecursive (scm:fx- x0 1)))))

  (scm:define letChain
    (scm:lambda (x0)
      (scm:let*
        ([a1 (scm:fx+ x0 x0)]
         [b2 (scm:fx+ a1 a1)]
         [c3 (scm:fx+ b2 b2)])
          (scm:fx+ (scm:fx+ (scm:fx+ a1 b2) c3) (scm:fx* c3 c3)))))

  (scm:define isOdd
    (scm:lambda (x0)
      (scm:if (scm:fx=? x0 1)
              #f
              (isEven (scm:fx- x0 1)))))

  (scm:define isEven
    (scm:lambda (x0)
      (scm:if (scm:fx=? x0 0)
              #t
              (scm:let ([_1 (scm:fx- x0 1)])
                (scm:if (scm:fx=? _1 1)
                        #f
                        (isEven (scm:fx- _1 1))))))))
