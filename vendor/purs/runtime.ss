#!r6rs
#!chezscheme
(library
  (purs runtime)
  (export
    list-cons
    record-set
    record-ref
    record-has
    record-remove
    array-length
    array-ref
    make-array
    boolean<?
    boolean<=?
    boolean>=?
    boolean>?
    string->pstring
    pstring-concat
    pstring=?
    pstring>?
    pstring>=?
    pstring<?
    pstring<=?)
  (import
    (chezscheme)
    (only (purs runtime pstring) string->pstring
                                    pstring-concat
                                    pstring=?
                                    pstring>?
                                    pstring>=?
                                    pstring<?
                                    pstring<=?)
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

  (define (record-has rec k)
    (let ([res (assq k rec)])
      (not (eq? res #f))))

  (define (record-ref rec k)
    (let ([res (assq k rec)])
      (if (not res)
        (raise-continuable
          (condition
            (make-error)
            (make-message-condition (format "Record key ~a not defined in ~a" k rec))))
        (cdr res))))

  (define (record-remove rec k)
    (if (null? rec)
      rec
      (if (eq? (caar rec) k)
        (cdr rec)
        (cons (car rec) (record-remove (cdr rec) k)))))

  (define (record-set rec k v)
    (cons (cons k v) (record-remove rec k)))


  ;
  ; Lists
  ;

  (define list-cons
    (lambda (x) (lambda (xs) (cons x xs))))


  )
