#!r6rs
(library (xitomatl ssax private-5-1 error)
  (export
    make-errorer)
  (import
    (rnrs))

  (define (make-errorer who)
    (lambda (msg . more)
      (error who
             (call-with-string-output-port
               (lambda (sop)
                 (for-each (lambda (x) (display x sop))
                           (cons msg more)))))))
)
