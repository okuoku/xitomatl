#!r6rs
(library (xitomatl control)
  (export
    aif)
  (import 
    (rnrs))
  
  (define-syntax aif
    (lambda (stx)
      (syntax-case stx ()
        [(_ var ve te fe)
         (identifier? #'var)
         #'(let ([var ve])
             (if var te fe))]
        [(_ var pred ve te fe) 
         (identifier? #'var)
         #'(let ([var ve])
             (if (pred var) te fe))])))
)
