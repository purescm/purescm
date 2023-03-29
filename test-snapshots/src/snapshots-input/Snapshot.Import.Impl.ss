(library (Snapshot.Import.Impl foreign) 
  (export addImpl) 
  (import (chezscheme))
  
  (define addImpl (lambda (x) (lambda (y) (+ x y)))))
