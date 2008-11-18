#!r6rs
(library (xitomatl lists compat)
  (export
    make-list last-pair)
  (import
    (rnrs)
    (only (xitomatl define) define/AV define/?)
    (only (xitomatl predicates) exact-non-negative-integer?)
    (only (scheme base) void))
  
  (define/? make-list
    (case-lambda/? 
      [(n) (make-list n (void))]
      [([n exact-non-negative-integer?] v)
       (let loop ([n n] [r '()])
         (if (= 0 n)
           r
           (loop (- n 1) (cons v r))))]))
  
  ;;; TODO! This must be cyclic structure / circular list safe!
  
  (define/? (last-pair [x pair?])
    (let loop ([y (cdr x)] [x x])
      (if (pair? y)
        (loop (cdr y) y)
        x)))
  
)
