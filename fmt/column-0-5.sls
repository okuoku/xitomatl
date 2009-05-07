;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl fmt column (0 5))
  (export
    fmt-columns
    columnar
    fold-lines
    wrap-fold-words
    wrap-fold
    wrap-lines
    justify
    fmt-file
    line-numbers)
  (import
    (except (rnrs) error)
    (only (rnrs r5rs) inexact->exact quotient remainder)
    (only (srfi :1 lists) fold)
    (srfi :6 basic-string-ports)
    (only (srfi :13 strings) string-concatenate string-concatenate-reverse
                             string-index string-tokenize substring/shared)
    (prefix (srfi :23 error) ER:)
    (only (srfi :39 parameters) parameterize)
    (xitomatl include)
    (xitomatl fmt base (0 5))
    (xitomatl fmt let-optionals*))
  
  (define (error . args)
    (parameterize ((ER:error-who "(library (xitomatl fmt column (0 5)))"))
      (apply ER:error args)))

  (define read-line
    (case-lambda
      (() (read-line (current-input-port)))
      ((tip) (get-line tip))))
  
  (include/resolve ("xitomatl" "fmt") "fmt-column.scm")
)
