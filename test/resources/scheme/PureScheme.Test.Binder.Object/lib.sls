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
            (scm:= (scm:hashtable-ref v "a" "Key not found: a") 1)
            (scm:= (scm:hashtable-ref v "b" "Key not found: b") 2)
            (scm:= (scm:hashtable-ref v "c" "Key not found: c") 3))
          4)
        ((scm:and
            (scm:= (scm:hashtable-ref v "d" "Key not found: d") 5)
            (scm:= (scm:hashtable-ref v "e" "Key not found: e") 6))
          7)
        (#t 8)
        (scm:else
          (scm:error #f "Failed pattern match LHVCPOGMPKVUZPCLQFRZXCZS")))))
  )