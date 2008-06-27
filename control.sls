#!r6rs
(library (xitomatl control)
  (export
    aif
    begin0)
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
  
  (define-syntax begin0
    (syntax-rules ()
      [(_ form0 form1 ...)
       (let ([result form0])
         (begin form1 ... result))]))
  
)
