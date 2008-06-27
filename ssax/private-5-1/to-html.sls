#!r6rs
(library (xitomatl ssax private-5-1 to-html)
  (export
    (rename (SXML->HTML sxml->html)            
            (string->goodHTML string->good-html))
    enattr
    entag)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl ssax tree-trans)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 util))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SXML-to-HTML.scm")
)
