(library (PureScheme.Test.Recursion.Mutual lib) (export foo bar) (import (rnrs)) (define foo (lambda (v) (lambda (v1) (cond ((= v 0) ((foo 1) v1)) ((= v 1) ((bar 2) v1)) ((= v 2) v1) (#t 0) (else (error #f "Failed pattern match"))))))

(define bar (lambda (v) (lambda (v1) (cond ((= v 0) ((bar 1) v1)) ((= v 1) ((foo 2) v1)) ((= v 2) v1) (#t 0) (else (error #f "Failed pattern match")))))))