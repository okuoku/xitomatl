#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl sxml-tools sxpath-plus (2008 06 27))
  (export
    analyze-red1
    analyze-reduce
    analyze-1
    analyze-step
    analyze-path
    sxpath+)
  (import
    (rename (rnrs) (syntax->datum syntax-object->datum))
    (xitomatl include))

  (include/resolve ("xitomatl" "sxml-tools") "sxpath-plus.scm")
)
