#!r6rs
(library (xitomatl ssax private-5-1 util)
  (export
    ;; Only the ones (xitomatl ssax ---) needs
    string->integer
    string-split
    make-char-quotator)
  (import
    (except (rnrs) error)
    (rnrs mutable-pairs)
    (xitomatl include)
    (xitomatl ssax private-5-1 error)
    (xitomatl ssax private-5-1 misc)
    (except (xitomatl srfi strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash))
  
  (define error (make-errorer "(xitomatl ssax private-5-1 util)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "util.scm")
)
