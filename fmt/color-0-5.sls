;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl fmt color (0 5))
  (export
    fmt-in-html fmt-color fmt-in-html? fmt-use-html-font?
    fmt-colored fmt-red fmt-blue fmt-green fmt-cyan fmt-yellow
    fmt-magenta fmt-black fmt-white fmt-bold fmt-underline)
  (import
    (except (rnrs) bitwise-ior bitwise-and)
    (xitomatl include)
    (xitomatl fmt base (0 5))
    (xitomatl fmt srfi-33))

  (include/resolve ("xitomatl" "fmt") "fmt-color.scm")
)
