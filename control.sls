#!r6rs
(library (xitomatl control)
  (export
    aif)
  (import 
    (rnrs))
  
  (define-syntax aif
    (syntax-rules ()
      [(_ var ve te fe) 
       (let ([t ve]) 
         (if t 
           (let ([var t]) te) 
           fe))]))
)
