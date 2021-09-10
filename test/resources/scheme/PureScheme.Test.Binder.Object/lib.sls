(library
  (PureScheme.Test.Binder.Object lib)
  (export foo)
  (import (rnrs))


  (define
    foo
    (lambda
      (v)
      (cond
        ((and
            (= (hashtable-ref v "a" (error #f "Key not found")) 1)
            (= (hashtable-ref v "b" (error #f "Key not found")) 2)
            (= (hashtable-ref v "c" (error #f "Key not found")) 3))
          4)
        ((and
            (= (hashtable-ref v "d" (error #f "Key not found")) 5)
            (= (hashtable-ref v "e" (error #f "Key not found")) 6))
          7)
        (#t 8)
        (else (error #f "Failed pattern match")))))
  )