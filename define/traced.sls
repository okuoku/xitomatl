(library (define traced)
  (export
    define
    lambda)
  (import
    (rename (rnrs) (define rnrs:define) (lambda rnrs:lambda))
    (only (ikarus) trace-define trace-lambda))
  
  (define-syntax define
    (syntax-rules ()
      [(_ (name . formals) expr0 expr* ...)
       (trace-define (name . formals) expr0 expr* ...)]
      [(_ name expr)
       (rnrs:define name expr)]
      [(_ name)
       (rnrs:define name)]))
  
  (define-syntax lambda
    (syntax-rules ()
      [(_ formals expr0 expr* ...)
       (trace-lambda <lambda> formals expr0 expr* ...)]))
    
)
