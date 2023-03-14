(library
  (PureScheme.Test.Let.ValuesInFunction lib)
  (export foo)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    foo
    (scm:lambda
      (x)
      (scm:lambda
        (y)
        (scm:letrec* ((b 2) (a 1)) (scm:fx- (scm:fx+ x a) (scm:fx+ y b))))))
  )