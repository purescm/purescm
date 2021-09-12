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
            (scm:= (scm:hashtable-ref v "a" (scm:error #f "Key not found")) 1)
            (scm:= (scm:hashtable-ref v "b" (scm:error #f "Key not found")) 2)
            (scm:= (scm:hashtable-ref v "c" (scm:error #f "Key not found")) 3))
          4)
        ((scm:and
            (scm:= (scm:hashtable-ref v "d" (scm:error #f "Key not found")) 5)
            (scm:= (scm:hashtable-ref v "e" (scm:error #f "Key not found")) 6))
          7)
        ((scm:and) 8)
        (scm:else (scm:error #f "Failed pattern match")))))
  )