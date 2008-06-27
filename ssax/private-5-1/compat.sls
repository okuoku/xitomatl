#!r6rs
(library (xitomatl ssax private-5-1 compat)
  (export
    with-output-to-string)
  (import
    (rnrs))

  (define (with-output-to-string thunk)
    (assertion-violation 'with-output-to-string 
                         "not available from this implementation"))
)
