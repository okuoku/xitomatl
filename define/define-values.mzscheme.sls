;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl define define-values)
  (export
    (rename (my:define-values define-values)))
  (import
    (rnrs)
    (only (scheme base) define-values))
  
  (define-syntax my:define-values
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id* ... . rid) expr)
         (identifier? #'rid)
         #'(define-values (id* ... rid)
             (let-values ([(id* ... . rid) expr])
               (values id* ... rid)))]
        [(_ . rest)
         #'(define-values . rest)])))
  
)
