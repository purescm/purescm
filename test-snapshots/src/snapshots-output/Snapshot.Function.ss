#!r6rs
#!chezscheme
(library
  (Snapshot.Function lib)
  (export
    f
    g)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define f
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:vector x0 y1 x0 y1 x0))))

  (scm:define g
    (scm:lambda (x0)
      (scm:lambda (y1)
        ((f x0) y1)))))
