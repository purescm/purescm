(library
  (PureScheme.Test.Record.Literal lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define
    foo
    (scm:letrec*
      (($ht (scm:make-hashtable scm:string-hash scm:string=? 2)))
      (scm:begin
        (scm:hashtable-set! $ht "bar" 23)
        (scm:hashtable-set! $ht "baz" 42)
        $ht)))
  )