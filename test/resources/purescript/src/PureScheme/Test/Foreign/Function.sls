(library (PureScheme.Test.Foreign.Function foreign)
  (export inc)
  (import (rnrs))

  (define inc
    (lambda (x)
      (1+ x))))
