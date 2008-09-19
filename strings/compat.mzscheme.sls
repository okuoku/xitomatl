#!r6rs
(library (xitomatl strings compat)
  (export
    string-copy!)
  (import
    (rnrs base)
    (prefix (only (scheme base) string-copy!) mz:))
  
  (define (string-copy! src src-start dst dst-start k)
    (mz:string-copy! dst dst-start src src-start (+ src-start k)))
  
)
