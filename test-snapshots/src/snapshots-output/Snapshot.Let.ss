#!r6rs
#!chezscheme
(library
  (Snapshot.Let lib)
  (export
    letChain)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define letChain
    (scm:lambda (x0)
      (scm:let*
        ([a1 (scm:fx+ x0 x0)]
        [b2 (scm:fx+ a1 a1)]
        [c3 (scm:fx+ b2 b2)])
          (scm:fx+ (scm:fx+ (scm:fx+ a1 b2) c3) (scm:fx* c3 c3))))))
