(library (purescm stack-trace)
  (export print-stack-trace)
  (import (chezscheme))

  ; Prints a stack trace from an exception
  ; 
  ; See discussion: <https://github.com/cisco/ChezScheme/issues/128>
  (define (print-stack-trace e)

    (define (get-func-name c)
      (let ([cc ((c 'code) 'name)])
        (if cc cc "--main--")))
    
    (display-condition e)
    (newline)
    
    (let loop ([insp (inspect/object (condition-continuation e))])
      (call/cc
        (lambda (ret)
          (when (fx>? (insp 'depth) 1)
            (call-with-values
              (lambda () (insp 'source-path))
              (case-lambda
                [(file line column)
                  (printf "  at ~a (~a:~a,~a)\n" (get-func-name insp) file line column)]
                [else (ret)]))
            (loop (insp 'link)))))))

  )
