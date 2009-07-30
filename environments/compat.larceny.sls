#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl environments compat)
  (export
    environment?
    environment-symbols)
  (import
    (rnrs base)
    (rename (rnrs lists) (for-all andmap))
    (only (rnrs eval reflection) environment-bindings))

  (define (environment? x)
    ;; NOTE: Probably not complete.
    ;; See: larceny_src/lib/R6RS/r6rs-expander.sch
    (and (pair? x)
         (list? (car x))
         (pair? (cdr x))
         (pair? (cadr x))
         (list? (caadr x))
         (andmap (lambda (y)
                   (and (pair? y)
                        (pair? (car y))))
                 (caadr x))))

  (define (environment-symbols env)
    (map cadar
         (environment-bindings env)))
)
