;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps ../lib
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a6/basic-string-ports.mzscheme.sls
#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (srfi :6 basic-string-ports)
  (export
    (rename (open-string-input-port open-input-string))
    open-output-string
    get-output-string)
  (import
    (rnrs)
    (only (scheme base) make-weak-hasheq hash-ref hash-set!))
  (define accumed-ht (make-weak-hasheq))
  (define (open-output-string)
    (letrec ([sop (make-custom-textual-output-port "string-output-port"
                    (lambda (string start count)
                      (when (positive? count)
                        (let ([al (hash-ref accumed-ht sop)])
                          (hash-set!
                            accumed-ht
                            sop
                            (cons
                              (substring string start (+ start count))
                              al))))
                      count)
                    #f #f #f)])
      (hash-set! accumed-ht sop '())
      sop))
  (define (get-output-string sop)
    (if (output-port? sop)
        (cond
          [(hash-ref accumed-ht sop #f) =>
           (lambda (al) (apply string-append (reverse al)))]
          [else
           (assertion-violation 'get-output-string
             "not a string-output-port"
             sop)])
        (assertion-violation 'get-output-string
          "not an output-port"
          sop))))
