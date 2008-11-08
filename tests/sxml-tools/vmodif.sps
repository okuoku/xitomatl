#!r6rs
(import
  (rnrs)
  (xitomatl sxml-tools modif)
  (xitomatl include)
  (xitomatl tests sxml-tools xtest-harness)
  (xitomatl tests sxml-tools xtest-lib)
  (xitomatl ssax private-5-1 output)
  (rename (only (xitomatl common) pretty-print)
          (pretty-print pp)))

(include/resolve ("xitomatl" "tests" "sxml-tools") "vmodif.scm")