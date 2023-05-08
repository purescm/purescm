;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../vendor/
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a128/comparators.sls
;;;;;; File header: %3a128/128.body1.scm
;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 
;;;;;; File header: %3a128/128.body2.scm
;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 
(library (purs runtime srfi :128 comparators)
  (export comparator? comparator-ordered? comparator-hashable?
   make-comparator make-pair-comparator make-list-comparator
   make-vector-comparator make-eq-comparator
   make-eqv-comparator make-equal-comparator boolean-hash
   char-hash char-ci-hash string-hash string-ci-hash
   symbol-hash number-hash make-default-comparator default-hash
   comparator-register-default! comparator-type-test-predicate
   comparator-equality-predicate comparator-ordering-predicate
   comparator-hash-function comparator-test-type
   comparator-check-type comparator-hash hash-bound hash-salt
   =? <? >? <=? >=? comparator-if<=>)
  (import (except (rnrs) define-record-type) (purs runtime srfi :99)
    (purs runtime srfi :39) (only (rnrs r5rs) modulo) (srfi private include))
  (define (exact-integer? x) (and (integer? x) (exact? x)))
  (begin
    (define-syntax comparator-if<=>
      (syntax-rules ()
        [(if<=> a b less equal greater)
         (comparator-if<=> (make-default-comparator) a b less equal
           greater)]
        [(comparator-if<=> comparator a b less equal greater)
         (cond
           [(=? comparator a b) equal]
           [(<? comparator a b) less]
           [else greater])]))
    (define-syntax hash-bound
      (syntax-rules () [(hash-bound) 33554432]))
    (define %salt% (make-parameter 16064047))
    (define-syntax hash-salt
      (syntax-rules () [(hash-salt) (%salt%)]))
    (define-syntax with-hash-salt
      (syntax-rules ()
        [(with-hash-salt new-salt hash-func obj)
         (parameterize ([%salt% new-salt]) (hash-func obj))]))
    (define-record-type comparator
      (make-raw-comparator type-test equality ordering hash
        ordering? hash?)
      comparator?
      (type-test comparator-type-test-predicate)
      (equality comparator-equality-predicate)
      (ordering comparator-ordering-predicate)
      (hash comparator-hash-function)
      (ordering? comparator-ordered?)
      (hash? comparator-hashable?))
    (define (make-comparator type-test equality ordering hash)
      (make-raw-comparator (if (eq? type-test #t) (lambda (x) #t) type-test)
        (if (eq? equality #t)
            (lambda (x y) (eqv? (ordering x y) 0))
            equality)
        (if ordering
            ordering
            (lambda (x y) (error #f "ordering not supported")))
        (if hash
            hash
            (lambda (x y) (error #f "hashing not supported")))
        (if ordering #t #f) (if hash #t #f)))
    (define (comparator-test-type comparator obj)
      ((comparator-type-test-predicate comparator) obj))
    (define (comparator-check-type comparator obj)
      (if (comparator-test-type comparator obj)
          #t
          (error #f "comparator type check failed" comparator obj)))
    (define (comparator-hash comparator obj)
      ((comparator-hash-function comparator) obj))
    (define (binary=? comparator a b)
      ((comparator-equality-predicate comparator) a b))
    (define (binary<? comparator a b)
      ((comparator-ordering-predicate comparator) a b))
    (define (binary>? comparator a b) (binary<? comparator b a))
    (define (binary<=? comparator a b)
      (not (binary>? comparator a b)))
    (define (binary>=? comparator a b)
      (not (binary<? comparator a b)))
    (define (=? comparator a b . objs)
      (let loop ([a a] [b b] [objs objs])
        (and (binary=? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))
    (define (<? comparator a b . objs)
      (let loop ([a a] [b b] [objs objs])
        (and (binary<? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))
    (define (>? comparator a b . objs)
      (let loop ([a a] [b b] [objs objs])
        (and (binary>? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))
    (define (<=? comparator a b . objs)
      (let loop ([a a] [b b] [objs objs])
        (and (binary<=? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))
    (define (>=? comparator a b . objs)
      (let loop ([a a] [b b] [objs objs])
        (and (binary>=? comparator a b)
             (if (null? objs) #t (loop b (car objs) (cdr objs))))))
    (define (boolean<? a b) (and (not a) b))
    (define (boolean-hash obj) (if obj (%salt%) 0))
    (define (char-hash obj)
      (modulo (* (%salt%) (char->integer obj)) (hash-bound)))
    (define (char-ci-hash obj)
      (modulo
        (* (%salt%) (char->integer (char-foldcase obj)))
        (hash-bound)))
    (define (number-hash obj)
      (cond
        [(nan? obj) (%salt%)]
        [(and (infinite? obj) (positive? obj)) (* 2 (%salt%))]
        [(infinite? obj) (* (%salt%) 3)]
        [(real? obj) (abs (exact (round obj)))]
        [else
         (+ (number-hash (real-part obj))
            (number-hash (imag-part obj)))]))
    (define (complex<? a b)
      (if (= (real-part a) (real-part b))
          (< (imag-part a) (imag-part b))
          (< (real-part a) (real-part b))))
    (define (symbol<? a b)
      (string<? (symbol->string a) (symbol->string b)))
    (define (make-eq-comparator)
      (make-comparator #t eq? #f default-hash))
    (define (make-eqv-comparator)
      (make-comparator #t eqv? #f default-hash))
    (define (make-equal-comparator)
      (make-comparator #t equal? #f default-hash))
    (define (make-hasher)
      (let ([result (%salt%)])
        (case-lambda
          [() result]
          [(n)
           (set! result (+ (modulo (* result 33) (hash-bound)) n))
           result])))
    (define (make-pair-comparator car-comparator cdr-comparator)
      (make-comparator
        (make-pair-type-test car-comparator cdr-comparator)
        (make-pair=? car-comparator cdr-comparator)
        (make-pair<? car-comparator cdr-comparator)
        (make-pair-hash car-comparator cdr-comparator)))
    (define (make-pair-type-test car-comparator cdr-comparator)
      (lambda (obj)
        (and (pair? obj)
             (comparator-test-type car-comparator (car obj))
             (comparator-test-type cdr-comparator (cdr obj)))))
    (define (make-pair=? car-comparator cdr-comparator)
      (lambda (a b)
        (and ((comparator-equality-predicate car-comparator)
               (car a)
               (car b))
             ((comparator-equality-predicate cdr-comparator)
               (cdr a)
               (cdr b)))))
    (define (make-pair<? car-comparator cdr-comparator)
      (lambda (a b)
        (if (=? car-comparator (car a) (car b))
            (<? cdr-comparator (cdr a) (cdr b))
            (<? car-comparator (car a) (car b)))))
    (define (make-pair-hash car-comparator cdr-comparator)
      (lambda (obj)
        (let ([acc (make-hasher)])
          (acc (comparator-hash car-comparator (car obj)))
          (acc (comparator-hash cdr-comparator (cdr obj)))
          (acc))))
    (define (norp? obj) (or (null? obj) (pair? obj)))
    (define (make-list-comparator element-comparator type-test
             empty? head tail)
      (make-comparator
        (make-list-type-test element-comparator type-test empty?
          head tail)
        (make-list=? element-comparator type-test empty? head tail)
        (make-list<? element-comparator type-test empty? head tail)
        (make-list-hash element-comparator type-test empty? head
          tail)))
    (define (make-list-type-test element-comparator type-test
             empty? head tail)
      (lambda (obj)
        (and (type-test obj)
             (let ([elem-type-test (comparator-type-test-predicate
                                     element-comparator)])
               (let loop ([obj obj])
                 (cond
                   [(empty? obj) #t]
                   [(not (elem-type-test (head obj))) #f]
                   [else (loop (tail obj))]))))))
    (define (make-list=? element-comparator type-test empty?
             head tail)
      (lambda (a b)
        (let ([elem=? (comparator-equality-predicate
                        element-comparator)])
          (let loop ([a a] [b b])
            (cond
              [(and (empty? a) (empty? b) #t)]
              [(empty? a) #f]
              [(empty? b) #f]
              [(elem=? (head a) (head b)) (loop (tail a) (tail b))]
              [else #f])))))
    (define (make-list<? element-comparator type-test empty?
             head tail)
      (lambda (a b)
        (let ([elem=? (comparator-equality-predicate
                        element-comparator)]
              [elem<? (comparator-ordering-predicate element-comparator)])
          (let loop ([a a] [b b])
            (cond
              [(and (empty? a) (empty? b) #f)]
              [(empty? a) #t]
              [(empty? b) #f]
              [(elem=? (head a) (head b)) (loop (tail a) (tail b))]
              [(elem<? (head a) (head b)) #t]
              [else #f])))))
    (define (make-list-hash element-comparator type-test empty?
             head tail)
      (lambda (obj)
        (let ([elem-hash (comparator-hash-function
                           element-comparator)]
              [acc (make-hasher)])
          (let loop ([obj obj])
            (cond
              [(empty? obj) (acc)]
              [else (acc (elem-hash (head obj))) (loop (tail obj))])))))
    (define (make-vector-comparator element-comparator type-test
             length ref)
      (make-comparator
        (make-vector-type-test
          element-comparator
          type-test
          length
          ref)
        (make-vector=? element-comparator type-test length ref)
        (make-vector<? element-comparator type-test length ref)
        (make-vector-hash element-comparator type-test length ref)))
    (define (make-vector-type-test element-comparator type-test
             length ref)
      (lambda (obj)
        (and (type-test obj)
             (let ([elem-type-test (comparator-type-test-predicate
                                     element-comparator)]
                   [len (length obj)])
               (let loop ([n 0])
                 (cond
                   [(= n len) #t]
                   [(not (elem-type-test (ref obj n))) #f]
                   [else (loop (+ n 1))]))))))
    (define (make-vector=? element-comparator type-test length
             ref)
      (lambda (a b)
        (and (= (length a) (length b))
             (let ([elem=? (comparator-equality-predicate
                             element-comparator)]
                   [len (length b)])
               (let loop ([n 0])
                 (cond
                   [(= n len) #t]
                   [(elem=? (ref a n) (ref b n)) (loop (+ n 1))]
                   [else #f]))))))
    (define (make-vector<? element-comparator type-test length
             ref)
      (lambda (a b)
        (cond
          [(< (length a) (length b)) #t]
          [(> (length a) (length b)) #f]
          [else
           (let ([elem=? (comparator-equality-predicate
                           element-comparator)]
                 [elem<? (comparator-ordering-predicate
                           element-comparator)]
                 [len (length a)])
             (let loop ([n 0])
               (cond
                 [(= n len) #f]
                 [(elem=? (ref a n) (ref b n)) (loop (+ n 1))]
                 [(elem<? (ref a n) (ref b n)) #t]
                 [else #f])))])))
    (define (make-vector-hash element-comparator type-test
             length ref)
      (lambda (obj)
        (let ([elem-hash (comparator-hash-function
                           element-comparator)]
              [acc (make-hasher)]
              [len (length obj)])
          (let loop ([n 0])
            (cond
              [(= n len) (acc)]
              [else (acc (elem-hash (ref obj n))) (loop (+ n 1))]))))))
  (begin
    (define unknown-object-comparator
      (make-comparator
        (lambda (obj) #t)
        (lambda (a b) #t)
        (lambda (a b) #f)
        (lambda (obj) 0)))
    (define first-comparator-index 9)
    (define *next-comparator-index* 9)
    (define *registered-comparators*
      (list unknown-object-comparator))
    (define (comparator-register-default! comparator)
      (set! *registered-comparators*
        (cons comparator *registered-comparators*))
      (set! *next-comparator-index*
        (+ *next-comparator-index* 1)))
    (define (object-type obj)
      (cond
        [(null? obj) 0]
        [(pair? obj) 1]
        [(boolean? obj) 2]
        [(char? obj) 3]
        [(string? obj) 4]
        [(symbol? obj) 5]
        [(number? obj) 6]
        [(vector? obj) 7]
        [(bytevector? obj) 8]
        [else (registered-index obj)]))
    (define (registered-index obj)
      (let loop ([i 0] [registry *registered-comparators*])
        (cond
          [(null? registry) (+ first-comparator-index i)]
          [(comparator-test-type (car registry) obj)
           (+ first-comparator-index i)]
          [else (loop (+ i 1) (cdr registry))])))
    (define (registered-comparator i)
      (list-ref
        *registered-comparators*
        (- i first-comparator-index)))
    (define (dispatch-equality type a b)
      (case type
        [(0) #t]
        [(1)
         ((make-pair=?
            (make-default-comparator)
            (make-default-comparator))
           a
           b)]
        [(2) (boolean=? a b)]
        [(3) (char=? a b)]
        [(4) (string=? a b)]
        [(5) (symbol=? a b)]
        [(6) (= a b)]
        [(7)
         ((make-vector=?
            (make-default-comparator)
            vector?
            vector-length
            vector-ref)
           a
           b)]
        [(8)
         ((make-vector=?
            (make-comparator exact-integer? = < default-hash)
            bytevector?
            bytevector-length
            bytevector-u8-ref)
           a
           b)]
        [else (binary=? (registered-comparator type) a b)]))
    (define (dispatch-ordering type a b)
      (case type
        [(0) 0]
        [(1)
         ((make-pair<?
            (make-default-comparator)
            (make-default-comparator))
           a
           b)]
        [(2) (boolean<? a b)]
        [(3) (char<? a b)]
        [(4) (string<? a b)]
        [(5) (symbol<? a b)]
        [(6) (complex<? a b)]
        [(7)
         ((make-vector<?
            (make-default-comparator)
            vector?
            vector-length
            vector-ref)
           a
           b)]
        [(8)
         ((make-vector<?
            (make-comparator exact-integer? = < default-hash)
            bytevector?
            bytevector-length
            bytevector-u8-ref)
           a
           b)]
        [else (binary<? (registered-comparator type) a b)]))
    (define (default-hash obj)
      (case (object-type obj)
        [(0 1 7) ((make-hasher) (equal-hash obj))]
        [(2) (boolean-hash obj)]
        [(3) (char-hash obj)]
        [(4) (string-hash obj)]
        [(5) (symbol-hash obj)]
        [(6) (number-hash obj)]
        [(8)
         ((make-vector-hash
            (make-default-comparator)
            bytevector?
            bytevector-length
            bytevector-u8-ref)
           obj)]
        [else
         (comparator-hash
           (registered-comparator (object-type obj))
           obj)]))
    (define (default-ordering a b)
      (let ([a-type (object-type a)] [b-type (object-type b)])
        (cond
          [(< a-type b-type) #t]
          [(> a-type b-type) #f]
          [else (dispatch-ordering a-type a b)])))
    (define (default-equality a b)
      (let ([a-type (object-type a)] [b-type (object-type b)])
        (if (= a-type b-type) (dispatch-equality a-type a b) #f)))
    (define (make-default-comparator)
      (make-comparator
        (lambda (obj) #t)
        default-equality
        default-ordering
        default-hash))))
