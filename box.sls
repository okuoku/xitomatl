#!r6rs
(library (xitomatl box)
  (export
    box box? make-box box-value box-value-set!)
  (import
    (rnrs))
  
  (define-record-type box
    (fields (mutable value)))
)
