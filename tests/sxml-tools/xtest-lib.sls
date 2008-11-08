#!r6rs
(library (xitomatl tests sxml-tools xtest-lib)
  (export
    xtest-filter
    xtest-ppw)
  (import
    (rnrs)
    (xitomatl include)
    (rename (only (xitomatl common) pretty-print)
            (pretty-print pp)))

  (include/resolve ("xitomatl" "tests" "sxml-tools") "xtest-lib.scm")
)
