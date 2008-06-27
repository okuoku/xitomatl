#!r6rs
(library (xitomatl ssax private-5-1 look-for-str)
  (export
    MISCIO:find-string-from-port?
    find-string-from-port?)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl ssax private-5-1 misc))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "look-for-str.scm")  
)
