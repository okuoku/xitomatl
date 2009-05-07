;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl conditionals)
  (export
    aif
    xor)
  (import 
    (rnrs))
  
  (define-syntax aif
    (lambda (stx)
      (syntax-case stx ()
        [(_ var ve te fe)
         (identifier? #'var)
         #'(let ([var ve])
             (if var te fe))]
        [(_ var pred ve te fe) 
         (identifier? #'var)
         #'(let ([var ve])
             (if (pred var) te fe))])))

  (define-syntax xor
    (syntax-rules ()
      ((_ expr ...)
       (xor-aux #F expr ...))))
  
  (define-syntax xor-aux
    (syntax-rules ()
      ((_ r)
       r)
      ((_ r expr)
       (let ((x expr))
         (if r
           (and (not x) r)
           x)))
      ((_ r expr0 expr ...)
       (let ((x expr0))
         (and (or (not r) (not x))
              (let ((n (or r x)))
                (xor-aux n expr ...)))))))
)
