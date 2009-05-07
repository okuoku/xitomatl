;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
