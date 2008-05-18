#!r6rs
(library (xitomatl predicates)
  (export
    exact-non-negative-integer?
    symbol<?
    non-empty-string?)
  (import
    (rnrs))

  (define (exact-non-negative-integer? x)
    (and (integer? x) (exact? x) (not (negative? x))))
  
  (define (symbol<? x y . r)
    (apply string<? (map symbol->string (cons* x y r))))
  
  (define (non-empty-string? x)
    (and (string? x) (positive? (string-length x))))
  
)
