#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl sxml-tools guides (2008 06 27))
  (export
    dgs:version
    dgs:fold
    dgs:find
    add-lp
    sxml-guide-flat
    sxml-guide
    xml-guide-flat)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (xitomatl include)
    (xitomatl ssax parsing)
    (xitomatl ssax private-5-1 output))

  (include/resolve ("xitomatl" "sxml-tools") "guides.scm")
)
