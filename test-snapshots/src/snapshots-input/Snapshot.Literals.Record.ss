(library (Snapshot.Literals.Record foreign) 
  (export minusTwo unsafeGetNotFound)
  (import (chezscheme)
          (prefix (purescm runtime) rt:))
  
  (define minusTwo -2)

  (define unsafeGetNotFound
    (lambda (r)
      (rt:record-ref r 'not-found)))


  )
