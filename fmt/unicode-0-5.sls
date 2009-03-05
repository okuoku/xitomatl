;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

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
