#!r6rs
(library (xitomatl debug)
  (export
    dprint dprint-mark
    format printf fprintf pretty-print)
  (import
    (rnrs)
    (xitomatl srfi parameters)
    (only (xitomatl common-unstandard) format printf fprintf pretty-print))
  
  (define dprint-mark (make-parameter "***"))
  
  (define-syntax dprint
    (lambda (stx)
      (syntax-case stx ()
        [(_ expr ...)
         (positive? (length #'(expr ...)))
         #'(let ([cep (current-error-port)]
                 [mark (dprint-mark)])
             (fprintf cep "~a\n" mark)           
             (let-values ([vs expr])
               (pretty-print 'expr cep)  ;; does newline
               (display "=>\n" cep)
               (for-each (lambda (v) (pretty-print v cep)) 
                         vs)
               (fprintf cep "~a\n" mark)           
               (apply values vs))
             ...)])))
  
)
