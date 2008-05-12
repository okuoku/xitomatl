#!r6rs
(library (xitomatl predicates)
  (export
    exact-non-negative-integer?
    symbol<?)
  (import
    (rnrs))

  (define (exact-non-negative-integer? x)
    (and (integer? x) (exact? x) (not (negative? x))))
  
  (define (symbol<? x y . r)
    (apply string<? (map symbol->string (cons* x y r))))
  
)
