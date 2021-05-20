(define min (lambda (n) (lambda (m) (cond ((((Data.Ord.lessThan Data.Ord.ordInt) n) m) n) (Data.Boolean.otherwise m)))))

(define gcd (lambda (v) (lambda (v1) (cond ((= v1 0) v) ((= v 0) v1) ((((Data.Ord.greaterThan Data.Ord.ordInt) v) v1) ((PureScmTest.GuardedFunction.gcd (- v v1)) v1)) (Data.Boolean.otherwise ((PureScmTest.GuardedFunction.gcd v) (- v1 v)))))))