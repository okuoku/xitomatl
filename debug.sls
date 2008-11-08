#!r6rs
(library (xitomatl debug)
  (export
    dprint dprint-mark
    eprint eprint-mark
    format printf fprintf pretty-print
    print-exception print-condition)
  (import
    (rnrs)
    (xitomatl srfi parameters)
    (only (xitomatl common-unstandard) format printf fprintf pretty-print)
    (only (xitomatl exceptions) print-exception)
    (only (xitomatl conditions) print-condition))
  
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
  
  (define eprint-mark (make-parameter "***"))
  
  (define (print-exn exn)
    (let ([cep (current-error-port)]
          [mark (eprint-mark)])
      (fprintf cep "~a\n" mark)
      (print-exception exn cep)
      (fprintf cep "~a\n" mark)
      (raise-continuable exn)))
  
  (define-syntax eprint
    (lambda (stx)
      (syntax-case stx ()
        [(_ expr ...)
         (positive? (length #'(expr ...)))
         #'(begin
             (let-values ([vs (with-exception-handler
                                print-exn
                                (lambda () #f expr))])
               (apply values vs))
             ...)])))
  
)
