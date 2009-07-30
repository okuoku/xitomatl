#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl macro-utils fib p-ctxt)
  (export
    p-ctxt)
  (import
    (for (prefix (only (rnrs base) define) p-)
         (meta -1) (meta 0))
    (for (prefix (only (rnrs syntax-case) syntax) p-)
         (meta -1) (meta 0)))

  (p-define p-ctxt (p-syntax here))
)
