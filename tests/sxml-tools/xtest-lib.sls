;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
