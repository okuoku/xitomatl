;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl sxml-tools stx-engine (2008 06 27))
  (export
    stx:version
    sxml:stylesheet
    stx:apply-templates
    stx:find-template)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl sxml-tools sxml-tools)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 error))

  (define stx:error (make-errorer "(xitomatl sxml-tools stx-engine)"))

  (include/resolve ("xitomatl" "sxml-tools") "stx-engine.scm")
)
