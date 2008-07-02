#!r6rs
(library (xitomatl sxml-match void)
  (export
    (rename (make-void void)))
  (import
    (rnrs))
  
  (define-record-type void)
)
