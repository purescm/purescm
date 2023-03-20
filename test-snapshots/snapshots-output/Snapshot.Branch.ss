#!r6rs
#!chezscheme
(library
  (Snapshot.Branch lib)
  (export
    f
    g)
  (import
    (prefix (chezscheme) scm:))

  (scm:define g
    (scm:lambda (v0)
      (scm:cond ((prim-op 1) (prim-op 2) (prim-op 3)) (scm:else 0))))

  (scm:define f
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:lambda (z2)
          (scm:cond ((x0 (scm:cond ((y1 (scm:cond ((z2 0)) (scm:else 1)))) (scm:else 2)))) (scm:else 3)))))))
