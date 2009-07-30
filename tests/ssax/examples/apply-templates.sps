#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (rnrs r5rs)
  (xitomatl include)
  (xitomatl ssax html)
  (xitomatl ssax sxpath)
  (xitomatl ssax private-5-1 output)
  (xitomatl ssax private-5-1 misc)
  (rename (only (xitomatl common) pretty-print) (pretty-print pp)))

(include/resolve ("xitomatl" "tests" "ssax" "examples") "apply-templates.scm")
