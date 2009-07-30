#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (xitomatl sxml-tools sxpathlib)
  (xitomatl include)
  (xitomatl sxml-tools sxpath)
  (xitomatl sxml-tools sxpath-ext)
  (xitomatl tests sxml-tools define-macro)
  (xitomatl ssax private-5-1 output))

(define (myenv:error . args)
  (apply assertion-violation 'sxp:error "test failed" args))

(include/resolve ("xitomatl" "tests" "sxml-tools") "vsxpathlib.scm")
