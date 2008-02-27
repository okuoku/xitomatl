(library (define traced)
  (export
    define
    lambda
    define-syntax)
  (import
    (rename (rnrs) (define rnrs:define) (lambda rnrs:lambda) (define-syntax rnrs:define-syntax))
    (only (ikarus) trace-define trace-lambda trace-define-syntax))
  
  (rnrs:define-syntax define
    (syntax-rules ()
      [(_ (name . formals) expr0 expr* ...)
       (trace-define (name . formals) expr0 expr* ...)]
      [(_ name expr)
       (rnrs:define name expr)]
      [(_ name)
       (rnrs:define name)]))
  
  (rnrs:define-syntax lambda
    (syntax-rules ()
      [(_ formals expr0 expr* ...)
       (trace-lambda <lambda> formals expr0 expr* ...)]))
  
  (rnrs:define-syntax define-syntax
    (syntax-rules ()
      [(_ name expr)
       (trace-define-syntax name expr)]))
    
)
