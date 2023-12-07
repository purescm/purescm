;;; Chez-Scheme library for Alex Shinn's Irregex
;;;
;;; Copyright (c) 2016 Federico Beffa <beffa@fbengineering.ch>
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(library (purs runtime irregex)
  (export 
    irregex
    string->irregex
    sre->irregex
    string->sre
    maybe-string->sre
    irregex?
    irregex-match-data?
    irregex-new-matches
    irregex-reset-matches!
    irregex-search
    irregex-search/matches
    irregex-match
    irregex-search/chunked
    irregex-match/chunked
    irregex-fold/chunked
    make-irregex-chunker
    irregex-match-substring
    irregex-match-subchunk
    irregex-match-start-chunk
    irregex-match-end-chunk
    irregex-match-start-index
    irregex-match-end-index
    irregex-match-num-submatches
    irregex-match-names
    irregex-match-valid-index?
    irregex-fold
    irregex-replace
    irregex-replace/all
    irregex-dfa
    irregex-dfa/search
    irregex-nfa
    irregex-flags
    irregex-lengths
    irregex-names
    irregex-num-submatches
    irregex-extract
    irregex-split
    sre->cset)
  (import 
    (except (rnrs)
            error
            find
            filter
            remove
            substring
            make-string
            string-ref
            string-length
            string?
            string=?
            string
            string-append
            string->list
            string->number
            number->string
            string->symbol
            string-foldcase
            string-ci=?)
    (rename (only (rnrs) string-append)
            (string-append native-string-append))
    (rnrs r5rs)
    (rnrs mutable-pairs)
    ;; (rnrs mutable-strings)
    (only (purs runtime bytestring) string->bytestring)
    (rename (purs runtime bytestring)
            (bytestring-ref string-ref)
            (bytestring-slice substring)
            (bytestring-length-code-units string-length)
            (make-bytestring make-string)
            (bytestring? string?)
            (bytestring=? string=?)
            (bytestring string)
            (bytestring-append string-append)
            (bytestring->list string->list)
            (bytestring->number string->number)
            (number->bytestring number->string)
            (bytestring->symbol string->symbol)
            (bytestring-foldcase string-foldcase)
            (bytestring-ci=? string-ci=?)
            (bytestring-cat-reverse string-cat-reverse))
    (only (chezscheme) include))

  ;; definition from irregex
  (define (error msg . args)
    (raise-continuable
      (apply
        condition
        (map make-message-condition (cons msg args)))))

  (include "irregex/irregex.scm")

)

