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
#|  
  (define (bytevector->string bv t)
    (call-with-port (open-bytevector-input-port bv t)
      (lambda (tcip)
        (let ([r (get-string-all tcip)])
          (if (eof-object? r) "" r)))))
  
  (define (string->bytevector str t)
    (call-with-bytevector-output-port
      (lambda (tcop)
        (put-string tcop str))
      t))
|#
)
