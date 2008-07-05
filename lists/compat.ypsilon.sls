(library (xitomatl lists compat)
  (export
    make-list last-pair)
  (import
    (rnrs)
    (only (xitomatl define extras) define/?)
    (only (core) make-list))
  
  (define/? (last-pair [x pair?])
    (let loop ([y (cdr x)] [x x])
      (if (pair? y)
        (loop (cdr y) y)
        x)))
  
)
