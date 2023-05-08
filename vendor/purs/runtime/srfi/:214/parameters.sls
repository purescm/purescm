;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../vendor/
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a214/parameters.sls
#!r6rs
(library (purs runtime srfi :214 parameters)
  (export
    flexvector-min-capacity
    flexvector-capacity-estimator)
  (import (rnrs) (purs runtime srfi :39))
  (define (assert-valid-capacity-value value)
    (assert (and (number? value) (> value 0)))
    value)
  (define flexvector-min-capacity
    (make-parameter 4 assert-valid-capacity-value))
  (define (assert-valid-capacity-estimator value)
    (assert (procedure? value))
    (let* ([example-current 4]
           [example-requested 8]
           [example-cap (value example-current example-requested)])
      (assert
        (and (number? example-cap)
             (exact? example-cap)
             (>= example-cap example-requested)))
      value))
  (define (multiple-of-current-estimator current requested)
    (assert
      (and (number? current)
           (number? requested)
           (> current 0)
           (> requested 0)))
    (let ([quot (exact (ceiling (/ requested current)))])
      (* quot current)))
  (define flexvector-capacity-estimator
    (make-parameter
      multiple-of-current-estimator
      assert-valid-capacity-estimator)))
