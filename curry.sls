;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl curry)
  (export
    define/curry
    curry)
  (import
    (rnrs)
    (only (xitomatl define) define/?)
    (only (xitomatl predicates) positive-integer?))
  
  (define-syntax define/curry
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name a ... . r) . body)
         (and (identifier? #'name)
              (positive? (length #'(a ...))))
         #`(define name
             (curry 
               (lambda (a ... . r) . body)
               #,(length #'(a ...))))])))
  
  (define/? (curry proc [n positive-integer?])
    (lambda args
      (let ([len (length args)])
        (if (>= len n)
          (apply proc args)
          (curry 
            (lambda more (apply proc (append args more))) 
            (- n len))))))
)
