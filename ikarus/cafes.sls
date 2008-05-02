(library (xitomatl ikarus cafes)
  (export
    transcript-cafe)
  (import
    (ikarus))
  
  (define transcript-cafe 
    (case-lambda
      [() (transcript-cafe "ikarus-transcript")]
      [(filename) (transcript-cafe filename eval)]
      [(filename evaler)
       (call-with-output-file filename
         (lambda (fop)
           (new-cafe
             (lambda (expr)
               (fprintf fop "> ~s\n" expr)
               (let-values ([vals 
                             (with-exception-handler
                               (lambda (ex)
                                 (display "Unhandled exception\n" fop)
                                 (print-condition ex fop)
                                 (flush-output-port fop)
                                 (raise ex))
                               (lambda ()
                                 (evaler expr (interaction-environment))))])
                 (for-each
                   (lambda (v)
                     (unless (eq? v (void))
                       (pretty-print v fop)))
                   vals)
                 (apply values vals))))))]))
  
)
