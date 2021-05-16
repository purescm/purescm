(define min (lambda (n) (lambda (m) (cond ((((Data.Ord.lessThan Data.Ord.ordInt) n1) m1) n) (Data.Boolean.otherwise m)))))

(define gcd (lambda (v) (lambda (v1) (cond ((= v1 0) v) ((= v 0) v1) ((((Data.Ord.greaterThan Data.Ord.ordInt) n) m) ((PureScmTest.GuardedFunction.gcd (- n m)) m)) (Data.Boolean.otherwise ((PureScmTest.GuardedFunction.gcd n) (- m n)))))))