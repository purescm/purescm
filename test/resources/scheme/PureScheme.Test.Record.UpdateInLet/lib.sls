(library
  (PureScheme.Test.Record.UpdateInLet lib)
  (export antani)
  (import (prefix (rnrs) scm:))


  (scm:define
    antani
    (scm:letrec*
      ((foo
        (scm:letrec*
          (($ht (scm:make-hashtable scm:string-hash scm:string=? 2)))
          (scm:begin
            (scm:hashtable-set! $ht "bar" 23)
            (scm:hashtable-set! $ht "baz" 42)
            $ht))))
      (scm:letrec*
        ((v foo))
        (scm:letrec*
          (($ht (scm:make-hashtable scm:string-hash scm:string=? 2)))
          (scm:begin
            (scm:hashtable-set! $ht "bar" 69)
            (scm:hashtable-set!
              $ht
              "baz"
              (scm:hashtable-ref v "baz" (scm:error #f "Key not found")))
            $ht)))))
  )