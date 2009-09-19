#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl tests fmt test)
  (export
    test test-error test-begin test-end)
  (import
    (rnrs)
    (srfi :78 lightweight-testing))
  
  (define-syntax test 
    (syntax-rules ()
      #;((_ name expected expr)
         (test expected expr))
      ((_ expected expr)
       (check expr => expected))))

  (define-syntax test-error
    (syntax-rules ()
      ((_ expr)
       (check (guard (ex (else (assertion-violation? ex)))
                expr
                'unexpected-return)
              => #T))))

  (define (test-begin _) #F)
  
  (define (test-end) (check-report))
)
