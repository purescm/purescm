;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../vendor/
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a23/error.sls
#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (purs runtime srfi :23 error)
  (export error)
  (import (rename (rnrs base) (error rnrs:error)))
  (define (error . args) (apply rnrs:error #f args)))
