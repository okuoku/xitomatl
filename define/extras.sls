#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl define extras)
  (export 
    (rename (my:define define)
            (my:define-syntax define-syntax)))
  (import
    (rnrs))
  
  (define-syntax my:define
    (syntax-rules ()
      ((_ ((maybe-pair . f1) . f2) expr expr* ...)
       (my:define (maybe-pair . f1)
         (lambda f2 expr expr* ...)))
      ((_ . rest)
       (define . rest))))
    
  (define-syntax my:define-syntax
    (syntax-rules ()
      ((_ (name . args) expr expr* ...)
       (define-syntax name
         (lambda args expr expr* ...)))
      ((_ . rest)
       (define-syntax . rest))))

)
