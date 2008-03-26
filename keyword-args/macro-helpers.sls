#!r6rs
(library (xitomatl keyword-args macro-helpers)
  (export
    check-kw-formals
    kw-arg*->ids)
  (import
    (rnrs)
    (xitomatl macro-utils))
  
  (define (kw-arg*->ids kw-arg*)
    (map (lambda (ka)
           (syntax-case ka () 
             [(kw de) #'kw]
             [kw #'kw]))
         kw-arg*))
  
  (define (check-kw-formals kw-frmls orig-stx)
    (formals-ok? 
      (syntax-case kw-frmls ()
        [(kw-arg* ... . kw-rest)
         (with-syntax ([(id* ...) (kw-arg*->ids #'(kw-arg* ...))])
           #'(id* ... . kw-rest))])
      orig-stx))
)
