#!r6rs
(library (xitomatl memoize)
  (export
    memoize
    define/memoize)
  (import
    (rnrs))
  
  ;; NOTE: Not currently thread/engine safe.  And of course, the client would
  ;;       need to ensure proc is thread/engine safe.
  
  (define (memoize proc)
    (let ([aal '()])
      (lambda args
        (cond
          [(assoc args aal)
           => (lambda (x) (apply values (cdr x)))]
          [else
           (let-values ([r (apply proc args)])
             (set! aal (cons (cons args r) aal))
             (apply values r))]))))
  
  (define-syntax define/memoize
    (syntax-rules ()
      [(_ (name . frmls) b0 b ...) 
       (identifier? #'name)
       (define name
         (memoize
           (lambda frmls b0 b ...)))]))

)
