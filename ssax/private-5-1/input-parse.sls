#!r6rs
(library (xitomatl ssax private-5-1 input-parse)
  (export
    peek-next-char
    assert-curr-char
    skip-until
    skip-while
    input-parse:init-buffer
    next-token-old
    next-token
    next-token-of
    *read-line-breaks*
    read-text-line
    read-string)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl ssax private-5-1 define-opt)
    (xitomatl ssax raise)
    (xitomatl ssax private-5-1 misc)
    (except (xitomatl srfi strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "input-parse.scm")
)
