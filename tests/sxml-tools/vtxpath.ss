#!/usr/bin/env scheme-script
#!r6rs
(import
  (rnrs)
  (xitomatl sxml-tools txpath)
  (xitomatl include)
  (xitomatl sxml-tools sxpathlib)
  (xitomatl tests sxml-tools xtest-harness)
  (xitomatl tests sxml-tools xtest-lib)
  (xitomatl ssax private-5-1 output)
  (rename (only (xitomatl common-unstandard) pretty-print)
          (pretty-print pp)))

(include/resolve ("xitomatl" "tests" "sxml-tools") "vtxpath.scm")