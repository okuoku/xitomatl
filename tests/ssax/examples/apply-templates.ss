#!/usr/bin/env/ scheme-script
#!r6rs
(import
  (rnrs)
  (rnrs r5rs)
  (xitomatl include)
  (xitomatl ssax html)
  (xitomatl ssax sxpath)
  (xitomatl ssax private-5-1 output)
  (xitomatl ssax private-5-1 misc)
  (rename (only (xitomatl common-unstandard) pretty-print) (pretty-print pp)))

(include/resolve ("xitomatl" "tests" "ssax" "examples") "apply-templates.scm")
