;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl lists compat)
  (export
    make-list last-pair)
  (import
    (rnrs)
    (only (xitomatl define) define/?))

  ;; chez alike make-list
  (define (make-list n . objs)
    (vector->list
      (if (pair? objs)
        (make-vector n (car objs))
        (make-vector n))))
  
  (define/? (last-pair (x pair?))
    (let loop ((y (cdr x)) (x x))
      (if (pair? y)
        (loop (cdr y) y)
        x)))
  
)
