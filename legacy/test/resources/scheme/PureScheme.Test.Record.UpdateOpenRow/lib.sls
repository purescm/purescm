(library
  (PureScheme.Test.Record.UpdateOpenRow lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define
    foo
    (scm:lambda
      (v)
      (scm:letrec*
        (($ht (scm:hashtable-copy v)))
        (scm:begin (scm:hashtable-set! $ht "bar" 42) $ht))))
  )