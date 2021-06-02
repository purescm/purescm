(define Foo (lambda (value0) (cons (quote Foo) (vector value0))))

(define Bar (lambda (value0) (cons (quote Bar) (vector value0))))

(define match (lambda (v) (cond ((and (eq? (car v) (quote Bar)) (eq? (car (vector-ref (cdr v) 0)) (quote Foo)) (= (vector-ref (cdr (vector-ref (cdr v) 0)) 0) 0)) 1) (#t 23))))