(define foo (lambda (v) (lambda (v1) (cond ((= v 0) ((PureScheme.Test.MutuallyRecursiveFunction.foo 1) v1)) ((= v 1) ((PureScheme.Test.MutuallyRecursiveFunction.bar 2) v1)) ((= v 2) v1) (#t 0)))))

(define bar (lambda (v) (lambda (v1) (cond ((= v 0) ((PureScheme.Test.MutuallyRecursiveFunction.bar 1) v1)) ((= v 1) ((PureScheme.Test.MutuallyRecursiveFunction.foo 2) v1)) ((= v 2) v1) (#t 0)))))