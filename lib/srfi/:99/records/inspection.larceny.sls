;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../lib
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a99/records/inspection.larceny.sls
(library (srfi :99 records inspection)
  (export record? record-rtd rtd-name rtd-parent
    rtd-field-names rtd-all-field-names rtd-field-mutable?)
  (import (err5rs records inspection)))
