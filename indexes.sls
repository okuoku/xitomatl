#!r6rs
(library (xitomatl indexes)
  (export
    iota
    enumerate)
  (import
    (rnrs))
  
  (define (_iota n l)
    (if (= n -1)
      l
      (_iota (- n 1) (cons n l))))
  
  (define (iota n)
    (unless (and (integer? n) (exact? n) (not (negative? n)))
      (assertion-violation 'iota "not an exact non-negative integer" n))
    (_iota (- n 1) '()))
  
  (define (enumerate x)
    (let ([len (cond [(list? x) length]
                     [(vector? x) vector-length]
                     [(string? x) string-length]
                     [(bytevector? x) bytevector-length]
                     [else (assertion-violation 'enumerate "invalid type" x)])])
      (_iota (- (len x) 1) '())))
  
)
