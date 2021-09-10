(library
  (PureScheme.Test.Constructor.Nested lib)
  (export Foo Bar)
  (import (rnrs))


  (define Foo (lambda (value0) (cons (quote Foo) (vector value0))))

  (define Bar (lambda (value0) (cons (quote Bar) (vector value0))))
  )