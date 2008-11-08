#!r6rs
(library (xitomatl tests sxml-tools define-macro)
  (export
    define-macro
    gensym)
  (import
    (rnrs)
    (only (xitomatl common) gensym))
  
  (define-syntax define-macro
    (syntax-rules ()
      [(_ (name . args) . body)
       (define-syntax name
         (lambda (stx)
           (define T 
             (case-lambda
               [args . body]
               [oops (syntax-violation #f "invalid syntax" stx)]))
           (syntax-case stx ()
             [(ctxt form (... ...))
              (datum->syntax #'ctxt
                (apply T (syntax->datum #'(form (... ...)))))])))]))
)
