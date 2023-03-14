(library
  (PureScheme.Test.Record.UpdateClosedRow lib)
  (export foo foo$prime)
  (import (prefix (rnrs) scm:))


  (scm:define
    foo
    (scm:letrec*
      (($ht (scm:make-hashtable scm:string-hash scm:string=? 2)))
      (scm:begin
        (scm:hashtable-set! $ht "bar" 23)
        (scm:hashtable-set! $ht "baz" 42)
        $ht)))

  (scm:define
    foo$prime
    (scm:letrec*
      ((v foo))
      (scm:letrec*
        (($ht (scm:make-hashtable scm:string-hash scm:string=? 2)))
        (scm:begin
          (scm:hashtable-set! $ht "bar" 69)
          (scm:hashtable-set! $ht "baz" 420)
          $ht))))
  )