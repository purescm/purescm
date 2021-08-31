(library (PureScheme.Test.Constructor.C1V2 lib) (export Bar Baz) (import (rnrs) (prefix (Prim lib) Prim.)) (define Bar (cons (quote Bar) (vector)))

(define Baz (cons (quote Baz) (vector))))