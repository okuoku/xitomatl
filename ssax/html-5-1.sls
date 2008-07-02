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
