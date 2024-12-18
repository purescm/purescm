;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../lib
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a126.sls
(library (srfi :126)
  (export make-eq-hashtable make-eqv-hashtable make-hashtable
   alist->eq-hashtable alist->eqv-hashtable alist->hashtable
   weakness hashtable? hashtable-size hashtable-ref
   hashtable-set! hashtable-delete! hashtable-contains?
   hashtable-lookup hashtable-update! hashtable-intern!
   hashtable-copy hashtable-clear! hashtable-empty-copy
   hashtable-keys hashtable-values hashtable-entries
   hashtable-key-list hashtable-value-list
   hashtable-entry-lists hashtable-walk hashtable-update-all!
   hashtable-prune! hashtable-merge! hashtable-sum
   hashtable-map->lset hashtable-find hashtable-empty?
   hashtable-pop! hashtable-inc! hashtable-dec!
   hashtable-equivalence-function hashtable-hash-function
   hashtable-weakness hashtable-mutable? hash-salt equal-hash
   string-hash string-ci-hash symbol-hash)
  (import (srfi :126 r6rs-hashtables)))
