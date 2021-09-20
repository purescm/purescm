(library
  (PureScheme.Test.Binder.Object lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define
    foo
    (scm:lambda
      (v)
      (scm:cond
        ((scm:and
            (scm:= (scm:hashtable-ref v "a" (scm:quote "Key not found: a")) 1)
            (scm:= (scm:hashtable-ref v "b" (scm:quote "Key not found: b")) 2)
            (scm:= (scm:hashtable-ref v "c" (scm:quote "Key not found: c")) 3))
          4)
        ((scm:and
            (scm:= (scm:hashtable-ref v "d" (scm:quote "Key not found: d")) 5)
            (scm:= (scm:hashtable-ref v "e" (scm:quote "Key not found: e")) 6))
          7)
        (#t 8)
        (scm:else (scm:error #f "Failed pattern match")))))
  )