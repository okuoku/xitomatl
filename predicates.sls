#!r6rs
(library (xitomatl predicates)
  (export
    exact-non-negative-integer?)
  (import
    (rnrs))

  (define (exact-non-negative-integer? x)
    (and (integer? x) (exact? x) (not (negative? x))))
)
