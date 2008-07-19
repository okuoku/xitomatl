#!r6rs
(library (xitomatl curry)
  (export
    define/curry
    curry)
  (import
    (rnrs)
    (only (xitomatl define extras) define/?)
    (only (xitomatl predicates) positive-integer?))
  
  (define-syntax define/curry
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name a ... . r) . body)
         (and (identifier? #'name)
              (positive? (length #'(a ...))))
         #`(define name
             (curry 
               (lambda (a ... . r) . body)
               #,(length #'(a ...))))])))
  
  (define/? (curry proc [n positive-integer?])
    (lambda args
      (let ([len (length args)])
        (if (>= len n)
          (apply proc args)
          (curry 
            (lambda more (apply proc (append args more))) 
            (- n len))))))
)
