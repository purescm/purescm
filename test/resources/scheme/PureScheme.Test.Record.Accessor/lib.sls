(library
  (PureScheme.Test.Record.Accessor lib)
  (export foo bar baz)
  (import (rnrs))


  (define
    foo
    (let
      (($ht (make-hashtable string-hash string=? 2)))
      (begin (hashtable-set! $ht "bar" 23) (hashtable-set! $ht "baz" 42) $ht)))

  (define baz (hashtable-ref foo "baz" (error #f "Key not found")))

  (define bar (hashtable-ref foo "bar" (error #f "Key not found")))
  )