#!r6rs
#!chezscheme
(library
  (Snapshot.Branch lib)
  (export
    f
    g
    h)
  (import
    (prefix (chezscheme) scm:))

  (scm:define h
    (scm:lambda (v0)
      (scm:cond ((scm:= v0 3.14) 3.14159) (scm:else 0.0))))

  (scm:define g
    (scm:lambda (v0)
      (scm:cond ((scm:= v0 0) 1) ((scm:= v0 1) 2) ((scm:= v0 2) 3) (scm:else 0))))

  (scm:define f
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:lambda (z2)
          (scm:cond (x0 (scm:cond (y1 (scm:cond (z2 0) (scm:else 1))) (scm:else 2))) (scm:else 3)))))))
