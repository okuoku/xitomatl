;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl ports compat)
  (export
    port-closed?)
  (import
    (rnrs base)
    (rnrs io ports)
    (only (xitomatl define) define/?))

  (define/? (port-closed? (p port?))
    (not (or (input-port? p) (output-port? p))))
)
