(library
  (PureScheme.Test.Let.TopLevelValues lib)
  (export foo)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (scm:define foo (scm:letrec* ((b 2) (a 1)) (scm:fx+ a b)))
  )