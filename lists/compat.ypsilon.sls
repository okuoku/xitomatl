(library (xitomatl lists compat)
  (export
    make-list last-pair)
  (import
    (rnrs)
    (only (xitomatl define) define/?)
    (only (core) make-list))
  
  ;;; TODO! This must be cyclic structure / circular list safe!
  
  (define/? (last-pair [x pair?])
    (let loop ([y (cdr x)] [x x])
      (if (pair? y)
        (loop (cdr y) y)
        x)))
  
)
