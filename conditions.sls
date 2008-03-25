#!r6rs
(library (xitomatl conditions)
  (export
    make-argument-name-condition argument-name-condition? condition-argument-name
    assertion-violation/conditions
    make-predicate-condition predicate-condition? condition-pred)
  (import
    (rnrs))  
      
  (define-condition-type &argument-name &condition
    make-argument-name-condition argument-name-condition?
    (n condition-argument-name))
  
  (define (assertion-violation/conditions who msg irrts . cndts)
    (with-exception-handler
      (lambda (ex)
        (raise (apply condition ex cndts)))
      (lambda ()
        (apply assertion-violation who msg irrts))))
      
  (define-condition-type &predicate &condition
    make-predicate-condition predicate-condition?
    (p condition-pred))  ;; condition-predicate already taken by (rnrs)
)
