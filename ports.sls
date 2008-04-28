#!r6rs
(library (xitomatl ports)
  (export
    read-all
    get-lines-all
    port-for-each
    port-map)
  (import
    (rnrs)
    (rnrs mutable-pairs))
  
  (define read-all
    (case-lambda 
      [(port)
       (port-map values read port)]
      [()
       (read-all (current-input-port))]))  
  
  (define get-lines-all
    (case-lambda
      [(port)
       (port-map values get-line port)]
      [()
       (get-lines-all (current-input-port))]))
  
  (define port-for-each
    (case-lambda
      [(proc reader port)
       (let ([x (reader port)])
         (unless (eof-object? x)
           (proc x)
           (port-for-each proc reader port)))]
      [(proc reader) 
       (port-for-each proc reader (current-input-port))]))
  
  (define port-map
    (case-lambda
      [(proc reader port)
       (let* ([a (cons #f '())] [t a])
         (port-for-each 
           (lambda (x) 
             (set-cdr! t (cons (proc x) '()))
             (set! t (cdr t))) 
           reader port)
         (cdr a))]
      [(proc reader) 
       (port-map proc reader (current-input-port))]))
)
