(library
  (PureScheme.Test.Let.ValuesInFunction lib)
  (export foo)
  (import
    (rnrs)
    (prefix (Data.Ring lib) Data.Ring.)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.))


  (define foo (lambda (x) (lambda (y) (let ((b 2) (a 1)) (- (+ x a) (+ y b))))))
  )