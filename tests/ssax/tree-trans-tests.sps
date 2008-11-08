#!r6rs
(import
  (except (rnrs) assert)
  (xitomatl ssax tree-trans)
  (xitomatl include)
  (xitomatl srfi lightweight-testing)
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
