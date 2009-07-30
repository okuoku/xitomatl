#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl ssax private-5-1 define-opt)
  (export
    define-opt)
  (import
    (except (rnrs) error)
    (xitomatl include)
    (xitomatl ssax private-5-1 error))
  
  (define error (make-errorer "(xitomatl ssax private-5-1 define-opt)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "define-opt.scm")  
)
