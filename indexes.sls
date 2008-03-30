#!r6rs
(library (xitomatl indexes)
  (export
    enumerate-indexes)
  (import
    (rnrs))
  
  (define (enumerate-indexes x)
    (define len 
      (cond [(list? x) length]
            [(vector? x) vector-length]
            [(string? x) string-length]
            [(bytevector? x) bytevector-length]
            [else (assertion-violation 'enumerate-indexes "invalid type" x)]))
    (do ([i (fx- (len x) 1) (fx- i 1)]
         [indexes '() (cons i indexes)])
      [(fx=? i -1)
       indexes]))
  
)
