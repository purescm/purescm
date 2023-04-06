#!r6rs
#!chezscheme
(library
  (Snapshot.Branch lib)
  (export
    f
    g
    h
    i)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define i
    (scm:lambda (v0)
      (scm:lambda (v11)
        (scm:cond
          (v0 (scm:not v11))
          ((scm:not v11) #t)
          (v11 #f)
          (scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match"))))))))

  (scm:define h
    (scm:lambda (v0)
      (scm:cond
        ((scm:fl=? v0 3.14) 3.14159)
        (scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match")))))))

  (scm:define g
    (scm:lambda (v0)
      (scm:cond
        ((scm:fx=? v0 0) 1)
        ((scm:fx=? v0 1) 2)
        ((scm:fx=? v0 2) 3)
        (scm:else 0))))

  (scm:define f
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:lambda (z2)
          (scm:cond
            (x0 (scm:cond
              (y1 (scm:cond
                (z2 0)
                (scm:else 1)))
              (scm:else 2)))
            (scm:else 3)))))))
