#!r6rs
(import 
  (rnrs)
  (xitomatl irregex)
  (xitomatl tests irregex test)
  (xitomatl include))

(include/resolve ("xitomatl" "tests" "irregex") "test-irregex-scsh.scm")

(test-exit 69)
