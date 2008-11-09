#!r6rs
(library (xitomatl define extras)
  (export 
    (rename (my:define define)
            (my:define-syntax define-syntax)))
  (import
    (rnrs))
  
  (define-syntax my:define
    (syntax-rules ()
      [(_ ((maybe-list . f1) . f2) expr expr* ...)
       (my:define (maybe-list . f1)
         (lambda f2 expr expr* ...))]
      [(_ . rest)
       (define . rest)]))
    
  (define-syntax my:define-syntax
    (syntax-rules ()
      [(_ (name . args) expr expr* ...)
       (define-syntax name
         (lambda args expr expr* ...))]
      [(_ . rest)
       (define-syntax . rest)]))

)
