;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl lists compat)
  (export
    make-list last-pair)
  (import
    (only (xitomatl define) define/?)
    (only (xitomatl predicates) exact-non-negative-integer?)
    (prefix (primitives make-list) larceny:)
    (primitives last-pair))

  (define/? make-list
    (case-lambda/?
      ((n) (make-list n #F))
      (((n exact-non-negative-integer?) v)
       (larceny:make-list n v))))
)
