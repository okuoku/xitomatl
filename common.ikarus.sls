#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time
    with-input-from-string with-output-to-string
    ;; TODO: add to as needed/appropriate
    )
  (import
    (ikarus))
)
