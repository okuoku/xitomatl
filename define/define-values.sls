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
        [(_ (id* ... . rid) expr)
         (formals-ok? #'(id* ... . rid) stx)
         (with-syntax ([(t* ...) (generate-temporaries #'(id* ...))]
                       [(rt ...) (generate-temporaries 
                                   (if (identifier? #'rid) '(1) '()))])
           #`(begin
               (define t*) ...
               (define rt) ...
               (define dummy 
                 (call-with-values 
                  (lambda () #f expr) ;; #f first to prevent internal defines
                  (case-lambda
                    [(id* ... . rid)
                     (set! t* id*) ...
                     (set! rt rid) ...
                     #f]
                    [otherwise 
                     (define-values-error #,(length #'(id* ...)) otherwise)])))
               (define id* 
                 (let ([v t*]) (set! t* #f) v)) 
               ...
               (define rid 
                 (let ([v rt]) (set! rt #f) v)) 
               ...))])))  
  
)
