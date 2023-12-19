(library (Snapshot.Literals.Record foreign) 
  (export minusTwo unsafeGetNotFound)
  (import (chezscheme)
          (prefix (purs runtime) rt:))
  
  (define minusTwo -2)

  (define unsafeGetNotFound
    (lambda (r)
      (rt:object-ref r "not-found")))


  )
