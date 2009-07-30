#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (except (rnrs) assert)
  (xitomatl include)
  (xitomatl ssax parsing)
  (xitomatl ssax tree-trans)
  (xitomatl ssax private-5-1 output)
  (xitomatl ssax private-5-1 misc)
  (xitomatl ssax private-5-1 util)
  (xitomatl ssax raise)
  (rename (only (xitomatl common) pretty-print gensym) (pretty-print pp))
  (rename (only (srfi :13 strings) string-null? string-index-right)
          (string-index-right string-rindex)))

(define-syntax assert
  (syntax-rules ()
    ((_ expr x ...)
     (unless expr
       (raise (condition (make-assertion-violation)
                         (make-irritants-condition (list x ...))))))))

(include/resolve ("xitomatl" "tests" "ssax" "examples") "daml-parse-unparse.scm")
