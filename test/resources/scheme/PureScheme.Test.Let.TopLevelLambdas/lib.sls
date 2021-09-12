(library
  (PureScheme.Test.Let.TopLevelLambdas lib)
  (export foo)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define
    foo
    (scm:let*
      ((sub (scm:lambda (x) (scm:lambda (y) (- x y))))
        (add (scm:lambda (x) (scm:lambda (y) (+ x y)))))
      ((add ((sub 1) 2)) ((add 3) 4))))
  )