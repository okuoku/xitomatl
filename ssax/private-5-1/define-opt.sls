#!r6rs
(library (xitomatl ssax private-5-1 define-opt)
  (export
    define-opt)
  (import
    (except (rnrs) error)
    (xitomatl include)
    (xitomatl ssax private-5-1 error))
  
  (define error (make-errorer "(xitomatl ssax private-5-1 define-opt)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "define-opt.scm")  
)
