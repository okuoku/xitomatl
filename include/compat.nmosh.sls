;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl include compat)
  (export
    search-paths stale-when (rename (read read-annotated)))
  (import
    (rnrs base)
    (primitives prefix-list)
    (only (rnrs io simple) read))

  (define (search-paths)
    prefix-list)

  ;; it's unused
  (define-syntax stale-when
    (syntax-rules ()
      ((_ when-expr . r)
       (begin . r))))
)
