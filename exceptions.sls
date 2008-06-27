#!r6rs
(library (xitomatl exceptions)
  (export
    warning)
  (import
    (rnrs))
  
  (define (warning who msg . irrts)
    (raise-continuable
      (condition
        (make-warning)
        (if who 
          (make-who-condition who)
          (condition))
        (make-message-condition msg)
        (if (positive? (length irrts))
          (make-irritants-condition irrts)
          (condition)))))    
)
