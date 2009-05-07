;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(import
  (except (rnrs) assert)
  (xitomatl ssax tree-trans)
  (xitomatl include)
  (srfi :78 lightweight-testing)
  (xitomatl ssax private-5-1 output)
  (only (xitomatl common) pretty-print))

(define-syntax assert
  (syntax-rules ()
    [(_ expr ...)
     (begin (check expr => #t) ...)]))

(define (pp x)
  (display "\nPretty Printed:\n")
  (pretty-print x)
  (newline))

(include/resolve ("xitomatl" "tests" "ssax") "vSXML-tree-trans.scm")

(check-report)
