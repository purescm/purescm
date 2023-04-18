;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../vendor
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a78/lightweight-testing/compat.ypsilon.sls
#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (srfi :78 lightweight-testing compat)
  (export check:write)
  (import (rnrs) (only (core) pretty-print))
  (define check:write
    (case-lambda
      [(x) (check:write x (current-output-port))]
      [(x p) (pretty-print x p) (newline p)])))
