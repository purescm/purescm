;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../vendor
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a41.sls
#!r6rs
;; Automatically generated by private/make-aliased-libraries.sps
(library (srfi :41)
  (export define-stream list->stream port->stream stream
   stream->list stream-append stream-car stream-cdr
   stream-concat stream-cons stream-constant stream-drop
   stream-drop-while stream-filter stream-fold stream-for-each
   stream-from stream-iterate stream-lambda stream-length
   stream-let stream-map stream-match stream-null stream-null?
   stream-of stream-pair? stream-range stream-ref
   stream-reverse stream-scan stream-take stream-take-while
   stream-unfold stream-unfolds stream-zip stream?)
  (import (srfi :41 streams)))
