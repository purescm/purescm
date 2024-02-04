#!r6rs
#!chezscheme
(library
  (purs runtime)
  (export
    list-cons
    object-set
    object-ref
    object-has
    object-delete
    array-length
    array-ref
    make-array
    boolean<?
    boolean<=?
    boolean>=?
    boolean>?)
  (import
    (chezscheme)
    (prefix (purs runtime srfi :214) srfi:214:))

  ;
  ; Booleans
  ;

  (define boolean->integer
    (lambda (x)
      (if x 1 0)))

  (define boolean>?
    (lambda (x)
      (lambda (y)
        (fx>? (boolean->integer x) (boolean->integer y)))))

  (define boolean>=?
    (lambda (x)
      (lambda (y)
        (fx>=? (boolean->integer x) (boolean->integer y)))))

  (define boolean<=?
    (lambda (x)
      (lambda (y)
        (fx<=? (boolean->integer x) (boolean->integer y)))))

  (define boolean<?
    (lambda (x)
      (lambda (y)
        (fx<? (boolean->integer x) (boolean->integer y)))))

  ;
  ; Arrays
  ;

  (define make-array srfi:214:flexvector)

  (define array-ref srfi:214:flexvector-ref)

  (define array-length srfi:214:flexvector-length)


  ;
  ; Records
  ;

  (define (object-has alist k)
    (let ([res (assq k alist)])
      (not (eq? res #f))))

  (define (object-ref alist k)
    (let ([res (assq k alist)])
      (if (not res)
        (raise-continuable
          (condition
            (make-error)
            (make-message-condition (format "Object key ~a not defined in ~a" k alist))))
        (cdr res))))

  (define (object-delete xs k)
    (if (null? xs)
      xs
      (if (eq? (caar xs) k)
        (cdr xs)
        (cons (car xs) (object-delete (cdr xs) k)))))

  (define (object-set alist k v)
    (cons (cons k v) (object-delete alist k)))

  ;
  ; Lists
  ;

  (define list-cons
    (lambda (x) (lambda (xs) (cons x xs))))


  )
