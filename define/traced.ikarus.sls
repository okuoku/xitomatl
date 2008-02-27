(library (xitomatl define traced)
  (export
    (rename
      (my:define define)
      (my:lambda lambda)
      (my:define-syntax define-syntax)))
  (import
    (rnrs)
    (only (ikarus) trace-define trace-lambda trace-define-syntax))
  
  (define-syntax my:define
    (syntax-rules ()
      [(_ (name . formals) expr0 expr* ...)
       (trace-define (name . formals) expr0 expr* ...)]
      [(_ name expr)
       (define name expr)]
      [(_ name)
       (define name)]))
  
  (define-syntax my:lambda
    (syntax-rules ()
      [(_ formals expr0 expr* ...)
       (trace-lambda <lambda> formals expr0 expr* ...)]))
  
  (define-syntax my:define-syntax
    (syntax-rules ()
      [(_ name expr)
       (trace-define-syntax name expr)]))
    
)
