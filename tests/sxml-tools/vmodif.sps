#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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