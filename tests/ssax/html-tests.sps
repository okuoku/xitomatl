#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (except (rnrs) assert)
  (xitomatl ssax html)
  (xitomatl ssax tree-trans)
  (xitomatl include)
  (srfi :78 lightweight-testing)
  (xitomatl ssax private-5-1 output)
  (xitomatl ssax private-5-1 misc))

(define-syntax assert
  (syntax-rules ()
    ((_ expr ...)
     (begin (check expr => #T) ...))))

(include/resolve ("xitomatl" "tests" "ssax") "vSXML-to-HTML.scm")

(check-report)
