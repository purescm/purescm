;;; Copyright (c) 2012 Aaron W. Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. 

(library (srfi private platform-features)
  (export expand-time-features run-time-features)
  (import (chezscheme) (srfi private OS-id-features))

(define (expand-time-features) '(chezscheme syntax-case))

(define (run-time-features)
  (OS-id-features (symbol->string (machine-type))
    '(("t" threads)
      ("a6" x86-64)
      ("i3" x86)
      ("le" linux posix)
      ("ob" openbsd posix bsd)
      ("fb" freebsd posix bsd)
      ("nb" netbsd posix bsd)
      ("osx" darwin posix)
      ("s2" solaris posix)
      ("nt" windows))))

)
