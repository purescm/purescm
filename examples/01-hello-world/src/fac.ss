(library (fac)
  (export factorial)
  (import (chezscheme))

  (define factorial
    (lambda (n)
      (if (= n 0)
          1
          (* n (factorial (- n 1))))))

  )
