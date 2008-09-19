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
         #`(begin
             (define t 
               (call-with-values 
                (lambda () #f expr) ;; #f first to prevent internal defines
                (case-lambda
                  [(id* ... . rid)
                   (list id* ... #,@(if (identifier? #'rid) (list #'rid) '()))]
                  [otherwise
                   (define-values-error #,(length #'(id* ...)) otherwise)])))
             (define id*
               (let ([v (car t)]) (set! t (cdr t)) v))
             ...
             #,@(if (identifier? #'rid)
                  (list #'(define rid
                            (let ([v (car t)]) (set! t (cdr t)) v)))
                  '()))])))  
  
)
