#!r6rs
(library (xitomatl tests sxml-tools xtest-harness)
  (export
    xtest-list=
    x-lambda-placeholder
    xtest-equal?
    xtest-sep-line
    xtest-assert
    xtest-assert-var
    xtest-assert-write
    xtest:diff)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl tests sxml-tools define-macro))
  
  (include/resolve ("xitomatl" "tests" "sxml-tools") "xtest-harness.scm")
)
