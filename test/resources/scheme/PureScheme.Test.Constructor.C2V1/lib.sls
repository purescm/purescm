(library
  (PureScheme.Test.Constructor.C2V1 lib)
  (export Bar Baz)
  (import (rnrs))


  (define Bar (lambda (value0) (cons (quote Bar) (vector value0))))

  (define Baz (lambda (value0) (cons (quote Baz) (vector value0))))
  )