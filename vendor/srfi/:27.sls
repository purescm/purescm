;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../vendor
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a27.sls
#!r6rs
;; Automatically generated by private/make-aliased-libraries.sps
(library (srfi :27)
  (export default-random-source make-random-source random-integer
    random-real random-source-make-integers
    random-source-make-reals random-source-pseudo-randomize!
    random-source-randomize! random-source-state-ref
    random-source-state-set! random-source?)
  (import (srfi :27 random-bits)))
