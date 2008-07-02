#!r6rs
(library (xitomatl htmlprag (0 16))
  (export
    shtml-named-char-id 
    shtml-numeric-char-id
    make-shtml-entity
    shtml-entity-value
    make-html-tokenizer
    tokenize-html
    shtml-token-kind
    parse-html/tokenizer
    html->sxml-0nf
    html->sxml-1nf
    html->sxml-2nf
    html->sxml 
    html->shtml
    write-shtml-as-html
    shtml->html)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (xitomatl include)
    (xitomatl srfi string-ports))
  
  (include/resolve ("xitomatl" "htmlprag") "htmlprag.scm")
)
