;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl ssax html (5 1))
  (export
    SXML->HTML
    string->goodHTML
    enattr
    entag   
    
    make-header
    make-navbar
    make-footer
    universal-conversion-rules
    universal-protected-rules
    alist-conv-rules
    find-Header
    generic-web-rules)
  (import
    (rnrs)
    (xitomatl ssax private-5-1 to-html)
    (xitomatl ssax private-5-1 to-html-ext))
)
