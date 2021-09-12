(library
  (PureScheme.Test.Record.UpdateClosedRow lib)
  (export foo foo')
  (import (prefix (rnrs) scm:))


  (scm:define
    foo
    (scm:let*
      (($ht (scm:make-hashtable scm:string-hash scm:string=? 2)))
      (scm:begin
        (scm:hashtable-set! $ht "bar" 23)
        (scm:hashtable-set! $ht "baz" 42)
        $ht)))

  (scm:define
    foo'
    (scm:let*
      ((v foo))
      (scm:let*
        (($ht (scm:make-hashtable scm:string-hash scm:string=? 2)))
        (scm:begin
          (scm:hashtable-set! $ht "bar" 69)
          (scm:hashtable-set! $ht "baz" 420)
          $ht))))
  )