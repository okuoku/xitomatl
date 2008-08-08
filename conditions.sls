#!r6rs
(library (xitomatl conditions)
  (export
    make-argument-name-condition argument-name-condition? condition-argument-name
    assertion-violation/conditions error/conditions
    make-predicate-condition predicate-condition? condition-pred
    make-port-position-condition port-position-condition? condition-port-position)
  (import
    (rnrs))  
      
  (define-condition-type &argument-name &condition
    make-argument-name-condition argument-name-condition?
    (n condition-argument-name))
  
  (define (assertion-violation/conditions who msg irrts . cndts)
    (raise 
     (apply condition
            (make-assertion-violation)
            (make-who-condition who)
            (make-message-condition msg)
            (make-irritants-condition irrts)
            cndts)))
  
  (define (error/conditions who msg irrts . cndts)
    (raise 
     (apply condition
            (make-error)
            (make-who-condition who)
            (make-message-condition msg)
            (make-irritants-condition irrts)
            cndts)))
      
  (define-condition-type &predicate &condition
    make-predicate-condition predicate-condition?
    (p condition-pred))  ;; condition-predicate already taken by (rnrs)
  
  (define-condition-type &port-position &condition
    make-port-position-condition port-position-condition?
    (pos condition-port-position))
)
