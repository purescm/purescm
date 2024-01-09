#!r6rs
#!chezscheme
(library
  (purs runtime)
  (export
    list-cons
    object-copy
    object-set!
    object-ref
    make-object
    array-length
    array-ref
    make-array
    boolean<?
    boolean<=?
    boolean>=?
    boolean>?
    string->bytestring
    bytestring-concat
    bytestring=?
    bytestring>?
    bytestring>=?
    bytestring<?
    bytestring<=?)
  (import
    (chezscheme)
    (only (purs runtime bytestring) string->bytestring
                                    bytestring-concat
                                    bytestring=?
                                    bytestring>?
                                    bytestring>=?
                                    bytestring<?
                                    bytestring<=?)
    (prefix (purs runtime srfi :214) srfi:214:))

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

  (define make-array srfi:214:flexvector)

  (define array-ref srfi:214:flexvector-ref)

  (define array-length srfi:214:flexvector-length)

  (define make-object
    (lambda args
      (let ([obj (make-hashtable symbol-hash eq?)])
        (for-each
          (lambda (pair)
            (symbol-hashtable-set! obj (car pair) (cdr pair)))
          args)
        obj)))

  (define nil (cons #f #f))
  (define (nil? obj) (eq? obj nil))

  (define object-ref
    (lambda (ht k)
      (let ([value (symbol-hashtable-ref ht k nil)])
        (if (nil? value)
          (raise-continuable
            (condition
              (make-error)
              (make-message-condition (format "Object key ~a not defined" k))))
          value))))

  (define object-set! symbol-hashtable-set!)

  (define object-copy
    (lambda (v)
      (hashtable-copy v #t)))

  (define list-cons
    (lambda (x) (lambda (xs) (cons x xs))))


  )
