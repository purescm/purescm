;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../lib
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a39/parameters.sls
#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (srfi :39 parameters)
  (export make-parameter parameterize)
  (import (rnrs))
  (define make-parameter
    (case-lambda
      [(val) (make-parameter val values)]
      [(val guard)
       (unless (procedure? guard)
         (assertion-violation 'make-parameter
           "not a procedure"
           guard))
       (let ([p (case-lambda [() val] [(x) (set! val (guard x))])])
         (p val)
         p)]))
  (define-syntax parameterize
    (lambda (stx)
      (syntax-case stx ()
        [(_ () b0 b ...) #'(let () b0 b ...)]
        [(_ ((p e) ...) b0 b ...)
         (with-syntax ([(tp ...) (generate-temporaries #'(p ...))]
                       [(te ...) (generate-temporaries #'(e ...))])
           #'(let ([tp p] ... [te e] ...)
               (let ([swap (lambda ()
                             (let ([t (tp)]) (tp te) (set! te t))
                             ...)])
                 (dynamic-wind swap (lambda () b0 b ...) swap))))]))))
