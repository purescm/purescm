(define Bar (lambda (value0) (lambda (value1) (cons (quote Bar) (vector value0 value1)))))

(define Baz (cons (quote Baz) (vector)))

(define match (lambda (v) (cond ((and (eq? (car v) (quote Bar)) (= (vector-ref v 1) 1) (= (vector-ref v 2) 2)) 0) ((and (eq? (car v) (quote Bar)) (= (vector-ref v 1) 1)) 1) ((and (eq? (car v) (quote Bar)) (= (vector-ref v 2) 2)) 2) ((eq? (car v) (quote Bar)) (vector-ref v 1)) ((eq? (car v) (quote Baz)) 0))))