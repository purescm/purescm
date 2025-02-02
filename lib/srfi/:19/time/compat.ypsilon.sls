;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../lib
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a19/time/compat.ypsilon.sls
#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (srfi :19 time compat)
  (export time-resolution timezone-offset current-time
    cumulative-thread-time cumulative-process-time
    cumulative-gc-time time-nanosecond time-second)
  (import
    (rnrs base)
    (only (core) microsecond microsecond->utc)
    (srfi :19 time not-implemented))
  (define time-resolution 1000)
  (define timezone-offset
    (let ([t (microsecond)])
      (/ (- t (microsecond->utc t)) 1000000)))
  (define (current-time)
    (let-values ([(d m) (div-and-mod (microsecond) 1000000)])
      (cons d (* m 1000))))
  (define time-nanosecond cdr)
  (define time-second car))
