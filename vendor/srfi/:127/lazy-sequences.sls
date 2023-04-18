;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../vendor
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a127/lazy-sequences.sls
;;;;;; File header: %3a127/lseqs-impl.scm
;; Helper returns #t if any element of list is null or #f if none
(library (srfi :127 lazy-sequences)
  (export generator->lseq lseq? lseq=? lseq-car lseq-first
   lseq-cdr lseq-rest lseq-ref lseq-take lseq-drop lseq-realize
   lseq->generator lseq-length lseq-append lseq-zip lseq-map
   lseq-for-each lseq-filter lseq-remove lseq-find
   lseq-find-tail lseq-take-while lseq-drop-while lseq-any
   lseq-every lseq-index lseq-member lseq-memq lseq-memv)
  (import (rnrs) (rnrs mutable-pairs) (srfi private include))
  (begin
    (define (any-null? list)
      (cond
        [(null? list) #f]
        [(null? (car list)) #t]
        [else (any-null? (cdr list))]))
    (define (gappend . args)
      (lambda ()
        (if (null? args)
            (eof-object)
            (let loop ([v ((car args))])
              (if (eof-object? v)
                  (begin
                    (set! args (cdr args))
                    (if (null? args) (eof-object) (loop ((car args)))))
                  v)))))
    (define (generator->lseq gen)
      (let ([value (gen)])
        (if (eof-object? value) '() (cons value gen))))
    (define (lseq-car lseq) (car lseq))
    (define (lseq-first lseq) (car lseq))
    (define (lseq-cdr lseq)
      (if (procedure? (cdr lseq))
          (let ([obj ((cdr lseq))])
            (cond
              [(eof-object? obj) (set-cdr! lseq '()) '()]
              [else
               (let ([result (cons obj (cdr lseq))])
                 (set-cdr! lseq result)
                 result)]))
          (cdr lseq)))
    (define (lseq-rest lseq) (lseq-cdr lseq))
    (define (lseq? obj)
      (cond
        [(null? obj) #t]
        [(not (pair? obj)) #f]
        [(procedure? (cdr obj)) #t]
        [else (lseq? (cdr obj))]))
    (define (lseq=? = lseq1 lseq2)
      (cond
        [(and (null? lseq1) (null? lseq2)) #t]
        [(or (null? lseq1) (null? lseq2)) #f]
        [(= (lseq-car lseq1) (lseq-car lseq2))
         (lseq=? = (lseq-cdr lseq1) (lseq-cdr lseq2))]
        [else #f]))
    (define (lseq-take lseq i)
      (generator->lseq
        (lambda ()
          (if (= i 0)
              (eof-object)
              (let ([result (lseq-car lseq)])
                (set! lseq (lseq-cdr lseq))
                (set! i (- i 1))
                result)))))
    (define (lseq-drop lseq i)
      (let loop ([i i] [lseq lseq])
        (if (= i 0) lseq (loop (- i 1) (lseq-cdr lseq)))))
    (define (lseq-ref lseq i) (lseq-car (lseq-drop lseq i)))
    (define (lseq-realize lseq)
      (let loop ([next lseq])
        (if (null? next) lseq (loop (lseq-cdr next)))))
    (define (lseq-length lseq) (length (lseq-realize lseq)))
    (define (lseq->generator lseq)
      (lambda ()
        (if (null? lseq)
            (eof-object)
            (let ([result (lseq-car lseq)])
              (set! lseq (lseq-cdr lseq))
              result))))
    (define (lseq-append . lseqs)
      (generator->lseq
        (apply gappend (map lseq->generator lseqs))))
    (define (safe-lseq-cdr obj)
      (if (null? obj) obj (lseq-cdr obj)))
    (define (lseq-map proc . lseqs)
      (generator->lseq
        (lambda ()
          (if (any-null? lseqs)
              (eof-object)
              (let ([result (apply proc (map lseq-car lseqs))])
                (set! lseqs (map safe-lseq-cdr lseqs))
                result)))))
    (define (lseq-zip . lseqs) (apply lseq-map list lseqs))
    (define (lseq-for-each proc . lseqs)
      (apply for-each proc (map lseq-realize lseqs)))
    (define (lseq-filter pred lseq)
      (generator->lseq
        (lambda ()
          (let loop ([lseq1 lseq])
            (if (null? lseq1)
                (eof-object)
                (let ([result (lseq-car lseq1)])
                  (cond
                    [(pred result) (set! lseq (lseq-cdr lseq1)) result]
                    [else (loop (lseq-cdr lseq1))])))))))
    (define (lseq-remove pred lseq)
      (lseq-filter (lambda (x) (not (pred x))) lseq))
    (define (lseq-find pred lseq)
      (cond
        [(null? lseq) #f]
        [(pred (lseq-car lseq)) (lseq-car lseq)]
        [else (lseq-find pred (lseq-cdr lseq))]))
    (define (lseq-find-tail pred lseq)
      (cond
        [(null? lseq) #f]
        [(pred (lseq-car lseq)) lseq]
        [else (lseq-find-tail pred (lseq-cdr lseq))]))
    (define (lseq-take-while pred lseq)
      (generator->lseq
        (lambda ()
          (if (not (pred (lseq-car lseq)))
              (eof-object)
              (let ([result (lseq-car lseq)])
                (set! lseq (lseq-cdr lseq))
                result)))))
    (define (lseq-drop-while pred lseq)
      (let loop ([lseq lseq])
        (if (not (pred (lseq-car lseq)))
            lseq
            (loop (lseq-cdr lseq)))))
    (define (lseq-any pred . lseqs)
      (let loop ([lseqs lseqs])
        (if (any-null? lseqs)
            #f
            (let ([result (apply pred (map lseq-car lseqs))])
              (if result result (loop (map lseq-cdr lseqs)))))))
    (define (lseq-every pred . lseqs)
      (let loop ([lseqs lseqs] [last-result #t])
        (if (any-null? lseqs)
            last-result
            (let ([result (apply pred (map lseq-car lseqs))])
              (if result (loop (map lseq-cdr lseqs) result) #f)))))
    (define (lseq-index pred . lseqs)
      (let loop ([lseqs lseqs] [n 0])
        (cond
          [(any-null? lseqs) #f]
          [(apply pred (map lseq-car lseqs)) n]
          [else (loop (map safe-lseq-cdr lseqs) (+ n 1))])))
    (define lseq-member
      (case-lambda
        [(x lseq) (lseq-member x lseq equal?)]
        [(x lseq =)
         (cond
           [(null? lseq) #f]
           [(= x (lseq-car lseq)) lseq]
           [else (lseq-member x (lseq-cdr lseq) =)])]))
    (define (lseq-memv x lseq) (lseq-member x lseq eqv?))
    (define (lseq-memq x lseq) (lseq-member x lseq eq?))))
