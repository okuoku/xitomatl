#!r6rs
(library (xitomatl ssax private-5-1 output)
  (export
    cout cerr nl)
  (import
    (rnrs)
    (xitomatl include))  

  (include/resolve ("xitomatl" "ssax" "private-5-1") "output.scm")
)
