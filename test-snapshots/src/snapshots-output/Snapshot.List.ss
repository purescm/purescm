#!r6rs
#!chezscheme
(library
  (Snapshot.List lib)
  (export
    car
    cdr
    curried
    xs)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:)
    (prefix (Data.List.Types lib) Data.List.Types.)
    (prefix (Data.Maybe lib) Data.Maybe.))

  (scm:define xs
    (scm:cons 1 (scm:quote ())))

  (scm:define curried
    (scm:lambda (x) (scm:lambda (xs) (scm:cons x xs))))

  (scm:define cdr
    (scm:lambda (v0)
      (scm:cond
        [(scm:null? v0) Data.Maybe.Nothing]
        [(scm:pair? v0) (Data.Maybe.Just (scm:cdr v0))]
        [scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match")))])))

  (scm:define car
    (scm:lambda (v0)
      (scm:cond
        [(scm:null? v0) Data.Maybe.Nothing]
        [(scm:pair? v0) (Data.Maybe.Just (scm:car v0))]
        [scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match")))]))))
