;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl curry)
  (export
    define/curry
    lambda/curry
    curry)
  (import
    (rnrs)
    (only (xitomatl define) define/?)
    (only (xitomatl predicates) non-negative-integer?))
  
  (define-syntax define/curry
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name a ... . r) . body)
         (identifier? #'name)
         #'(define name
             (lambda/curry (a ... . r) . body))])))
  
  (define-syntax lambda/curry
    (lambda (stx)
      (syntax-case stx ()
        ((_ (a a* ... . r) . body)
         #`(curry 
            (lambda (a a* ... . r) . body)
            #,(length #'(a a* ...))))
        ((_ . r)  ;; zero or "rest"-only arguments
         #'(lambda . r)))))

  (define/? (curry proc [n non-negative-integer?])
    (lambda args
      (let ([len (length args)])
        (if (>= len n)
          (apply proc args)
          (curry 
            (lambda more (apply proc (append args more))) 
            (- n len))))))
)
