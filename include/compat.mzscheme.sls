;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl include compat)
  (export
    search-paths stale-when (rename (read read-annotated)))
  (import
    (rnrs base)
    (only (rnrs io simple) read)
    (only (scheme base) current-library-collection-paths path->string)
    (only (scheme mpair) list->mlist))

  (define (search-paths)
    (map path->string 
         (list->mlist (current-library-collection-paths))))

  (define-syntax stale-when
    (syntax-rules ()
      ((_ when-expr . r)
       (begin . r))))
)
