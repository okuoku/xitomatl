#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (rnrs mutable-pairs)
  (rnrs r5rs) 
  (srfi :26 cut)
  (only (xitomatl control) compose)
  (only (xitomatl common) with-input-from-string)
  (only (xitomatl strings) string-split)
  (xitomatl include)
  (xitomatl fmt)
  (xitomatl tests fmt test))

(define-syntax cond-expand (syntax-rules () ((_ . _) (begin))))

(define-syntax define-macro
  (syntax-rules ()
    ((_ (name arg) . body)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           ((ctxt x)
            (datum->syntax #'ctxt
             ((lambda (arg) . body)
              (syntax->datum #'x))))))))))

(define (feature? x) (case x ((full-numeric-tower) #T) (else #F)))

(include/resolve ("xitomatl" "tests" "fmt") "test-fmt.scm")
