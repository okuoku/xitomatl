#!r6rs
(library (xitomatl port-utils)
  (export
    read-all)
  (import
    (rnrs))
  
  (define read-all
    (case-lambda 
      [(input-port)
       (let loop ([ls '()])
         (define x (read input-port))
         (if (eof-object? x)
           (reverse ls)
           (loop (cons x ls))))]
      [()
       (read-all (current-input-port))]))  
)
