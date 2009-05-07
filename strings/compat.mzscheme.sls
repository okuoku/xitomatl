;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
