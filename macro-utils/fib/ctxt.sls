;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl macro-utils fib ctxt)
  (export
    ctxt)
  (import
    (for (only (rnrs base) define)
         (meta -1) (meta 0))
    (for (only (rnrs syntax-case) syntax)
         (meta -1) (meta 0)))

  (define ctxt #'here)
)
