#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
      ((n) (make-list n (void)))
      (((n exact-non-negative-integer?) v)
       (let loop ((n n) (r '()))
         (if (= 0 n)
           r
           (loop (- n 1) (cons v r)))))))
  
  (define/? (last-pair (x pair?))
    (let loop ((y (cdr x)) (x x))
      (if (pair? y)
        (loop (cdr y) y)
        x)))
  
)
