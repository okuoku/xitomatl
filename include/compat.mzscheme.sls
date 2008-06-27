#!r6rs
(library (xitomatl include compat)
  (export
    search-paths)
  (import
    (rnrs base)
    (only (scheme base) current-library-collection-paths path->string)
    (only (scheme mpair) list->mlist))

  (define (search-paths)
    (map path->string 
         (list->mlist (current-library-collection-paths))))
)
