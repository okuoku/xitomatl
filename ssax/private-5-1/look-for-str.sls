#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
