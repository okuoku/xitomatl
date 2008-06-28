#!/usr/bin/env/ scheme-script
#!r6rs
(import
  (except (rnrs) assert)
  (xitomatl include)
  (xitomatl ssax parsing)
  (xitomatl ssax tree-trans)
  (prefix (xitomatl ssax parsing) ssax:)
  (xitomatl ssax private-5-1 output)
  (xitomatl ssax private-5-1 misc)
  (xitomatl ssax private-5-1 util)
  (xitomatl ssax raise)
  (rename (only (xitomatl common-unstandard) pretty-print gensym) (pretty-print pp))
  (rename (only (xitomatl srfi strings) string-null? string-index-right)
          (string-index-right string-rindex)))

(define-syntax assert
  (syntax-rules ()
    [(_ expr x ...)
     (unless expr
       (raise (condition (make-assertion-violation)
                         (make-irritants-condition (list x ...)))))]))

(include/resolve ("xitomatl" "tests" "ssax" "examples") "daml-parse-unparse.scm")
