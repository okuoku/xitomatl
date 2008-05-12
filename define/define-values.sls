#!r6rs
(library (xitomatl define define-values)
  (export
    define-values)
  (import
    (rnrs)
    (for (only (xitomatl macro-utils) formals-ok?) expand)
    (only (xitomatl common-unstandard) format))
  
  (define (define-values-error expected received-vals)
    (apply assertion-violation 'define-values
      (format "expected ~a values, received ~a values" expected (length received-vals))
      received-vals))
  
  (define-syntax define-values
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id* ...) expr)
         (formals-ok? #'(id* ...) stx)  ;; prevents duplicates
         (with-syntax ([(t* ...) (generate-temporaries #'(id* ...))])
           #`(begin
               (define t*) ...
               (define dummy 
                 (call-with-values 
                  (lambda () #f expr) ;; #f first to prevent internal defines
                  (case-lambda
                    [(id* ...)
                     (set! t* id*) ...
                     #f]
                    [otherwise 
                     (define-values-error #,(length #'(id* ...)) otherwise)])))
               (define id* 
                 (let ([v t*]) (set! t* #f) v)) 
               ...))])))  
  
)
