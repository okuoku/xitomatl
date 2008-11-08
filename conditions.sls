#!r6rs
(library (xitomatl conditions)
  (export
    make-argument-name-condition argument-name-condition? condition-argument-name
    make-predicate-condition predicate-condition? condition-pred
    make-port-position-condition port-position-condition? condition-port-position
    print-condition)
  (import
    (rnrs)
    (xitomatl conditions print-condition))  
      
  (define-condition-type &argument-name &condition
    make-argument-name-condition argument-name-condition?
    (n condition-argument-name))
      
  (define-condition-type &predicate &condition
    make-predicate-condition predicate-condition?
    (p condition-pred))  ;; condition-predicate already taken by (rnrs)
  
  (define-condition-type &port-position &condition
    make-port-position-condition port-position-condition?
    (pos condition-port-position))
)
