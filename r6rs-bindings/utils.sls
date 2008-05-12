#!r6rs
(library (xitomatl r6rs-bindings utils)
  (export
    all-libraries-names
    macros-of variables-of identifiers-of)
  (import
    (rnrs)
    (xitomatl r6rs-bindings spec)
    (only (xitomatl predicates) symbol<?))
  
  (define (all-libraries-names)
    (map car spec))
  
  (define (things-of libname type)
    (cond [(assoc libname spec)
           => (lambda (p) (cdr (assoc type (cdr p))))]
          [else #f]))
  
  (define (macros-of libname) 
    (list-sort symbol<? (things-of libname 'macros)))
  
  (define (variables-of libname) 
    (list-sort symbol<? (things-of libname 'variables)))
  
  (define (identifiers-of libspec) 
    (list-sort symbol<? (append (things-of libspec 'macros) (things-of libspec 'variables))))
)
