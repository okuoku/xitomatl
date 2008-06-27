#!r6rs
(library (xitomatl ssax private-5-1 to-html-ext)
  (export
    make-header
    make-navbar
    make-footer
    universal-conversion-rules
    universal-protected-rules
    alist-conv-rules
    find-Header
    generic-web-rules)
  (import
    (except (rnrs) error)
    (rnrs r5rs)
    (xitomatl include)
    (xitomatl ssax tree-trans)
    (rename (xitomatl ssax private-5-1 to-html)
            (sxml->html SXML->HTML)            
            (string->good-html string->goodHTML))
    (xitomatl ssax private-5-1 error)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 util)
    (except (xitomatl srfi strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash)
    (only (xitomatl file-system base) file-regular?))
  
  (define (OS:file-length filename)
    (if (file-regular? filename) 1 0))
  
  (define error (make-errorer "(xitomatl ssax private-5-1 to-html-ext)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SXML-to-HTML-ext.scm")
)
