#!r6rs
(import 
  (rename (rnrs) (substring rnrs:substring))
  (only (xitomatl indexes) iota)
  (xitomatl irregex)
  (xitomatl tests irregex test)
  (xitomatl include))

(define substring
  (case-lambda
    [(str s) (rnrs:substring str s (string-length str))]
    [(str s e) (rnrs:substring str s e)]))

(include/resolve ("xitomatl" "tests" "irregex") "test-irregex-gauche.scm")

(test-exit 308)
