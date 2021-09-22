(library
  (PureScheme.Test.TypeClass.Semigroupoid lib)
  (export compose semigroupoidFn)
  (import (prefix (rnrs) scm:))


  (scm:define Semigroupoid$Dict (scm:lambda (x) x))

  (scm:define
    semigroupoidFn
    (scm:letrec*
      (($ht (scm:make-hashtable scm:string-hash scm:string=? 1)))
      (scm:begin
        (scm:hashtable-set!
          $ht
          "compose"
          (scm:lambda (f) (scm:lambda (g) (scm:lambda (x) (f (g x))))))
        $ht)))

  (scm:define
    compose
    (scm:lambda
      (dict)
      (scm:cond
        (#t (scm:hashtable-ref dict "compose" "Key not found: compose"))
        (scm:else (scm:error #f "Failed pattern match")))))
  )