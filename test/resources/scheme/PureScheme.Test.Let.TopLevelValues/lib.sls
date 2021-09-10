(library
  (PureScheme.Test.Let.TopLevelValues lib)
  (export foo)
  (import
    (rnrs)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (define foo (let ((b 2) (a 1)) (+ a b)))
  )