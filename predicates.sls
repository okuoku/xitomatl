#!r6rs
(library (xitomatl predicates)
  (export
    exact-non-negative-integer?
    positive-integer?
    symbol<?
    non-empty-string?
    list-of?)
  (import
    (rnrs))

  (define (exact-non-negative-integer? x)
    (and (integer? x) (exact? x) (not (negative? x))))
  
  (define (positive-integer? x)
    (and (integer? x) (positive? x)))
  
  (define (symbol<? x y . r)
    (apply string<? (map symbol->string (cons* x y r))))
  
  (define (non-empty-string? x)
    (and (string? x) (positive? (string-length x))))
  
  (define (list-of? pred)
    (letrec ([list-of?-pred
              (lambda (x)
                (cond [(pair? x) (and (pred (car x)) (list-of?-pred (cdr x)))]
                      [(null? x) #t]
                      [else #f]))])
      list-of?-pred))
  
)
