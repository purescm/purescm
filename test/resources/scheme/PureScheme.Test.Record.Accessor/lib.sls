(library
  (PureScheme.Test.Record.Accessor lib)
  (export foo bar baz)
  (import (prefix (rnrs) scm:))


  (scm:define
    foo
    (scm:letrec*
      (($ht (scm:make-hashtable scm:string-hash scm:string=? 2)))
      (scm:begin
        (scm:hashtable-set! $ht "bar" 23)
        (scm:hashtable-set! $ht "baz" 42)
        $ht)))

  (scm:define baz (scm:hashtable-ref foo "baz" "Key not found: baz"))

  (scm:define bar (scm:hashtable-ref foo "bar" "Key not found: bar"))
  )