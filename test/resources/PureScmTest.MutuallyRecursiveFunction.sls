(define foo (lambda (v) (lambda (v1) (cond ((= v 0) ((PureScmTest.MutuallyRecursiveFunction.foo 1) v1)) ((= v 1) ((PureScmTest.MutuallyRecursiveFunction.bar 2) v1)) ((= v 2) v1) (#t 0)))))

(define bar (lambda (v) (lambda (v1) (cond ((= v 0) ((PureScmTest.MutuallyRecursiveFunction.bar 1) v1)) ((= v 1) ((PureScmTest.MutuallyRecursiveFunction.foo 2) v1)) ((= v 2) v1) (#t 0)))))