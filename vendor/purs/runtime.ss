#!r6rs
#!chezscheme
(library
  (purs runtime)
  (export
    kons
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
    boolean>?)
  (import
    (chezscheme)
    (prefix (purs runtime srfi :125) srfi:125:)
    (prefix (purs runtime srfi :128) srfi:128:)
    (prefix (purs runtime srfi :214) srfi:214:))

  (define boolean->integer
    (lambda (x)
      (if x 1 0)))

  (define string-comparator
    (srfi:128:make-comparator string? string=? string<? string-hash))

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
    (lambda args (srfi:125:alist->hash-table args string-comparator)))

  (define object-ref srfi:125:hash-table-ref)

  (define object-set! srfi:125:hash-table-set!)

  (define object-copy
    (lambda (v)
      (srfi:125:hash-table-copy v #t)))

  (define kons
    (lambda (x) (lambda (xs) (cons x xs))))


  )
