#!r6rs
(library (xitomatl bytevectors)
  (export
    bytevector-append)
  (import
    (rnrs)
    (only (xitomatl define extras) define/?)
    (only (xitomatl predicates) list-of?))
  
  (define/? (bytevector-append . #(bvs (list-of? bytevector?)))
    (let* ([lens (map bytevector-length bvs)]
           [n (make-bytevector (apply + lens))])
      (let loop ([bvs bvs] [lens lens] [npos 0])
        (cond 
          [(null? bvs) n]
          [else (bytevector-copy! (car bvs) 0 n npos (car lens))
                (loop (cdr bvs) (cdr lens) (+ npos (car lens)))]))))
)
