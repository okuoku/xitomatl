;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl fmt unicode (0 5))
  (export
    unicode-char-width
    unicode-string-width
    fmt-unicode)
  (import
    (rename (except (rnrs) bitwise-ior bitwise-and)
            (bytevector-u8-ref u8vector-ref))
    (only (rnrs r5rs) quotient remainder)
    (xitomatl include)
    (xitomatl fmt base (0 5))
    (xitomatl fmt srfi-33))

  (define-syntax u8vector
    (lambda (stx)
      (syntax-case stx ()
        ((_ u8s ...)
         #`(quote #,(u8-list->bytevector (syntax->datum #'(u8s ...))))))))

  (include/resolve ("xitomatl" "fmt") "fmt-unicode.scm")
)
