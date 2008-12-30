;;; Copyright (c) 2008 Derick Eddington
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; Except as contained in this notice, the name(s) of the above copyright
;;; holders shall not be used in advertising or otherwise to promote the sale,
;;; use or other dealings in this Software without prior written authorization.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!r6rs
(import
  (rnrs)
  (xitomatl library-utils)
  (xitomatl srfi lightweight-testing)
  (only (xitomatl exceptions) catch))

(define-syntax check-AV
  (syntax-rules ()
    [(_ expr)
     (check (catch ex ([else (assertion-violation? ex)])
              expr
              'unexpected-return)
            => #T)]))

;; library-name-without-version
(check (library-name-without-version '(foo zab (0 4))) => '(foo zab))
(check (library-name-without-version '(foo zab ())) => '(foo zab))
(check (library-name-without-version '(foo zab)) => '(foo zab))
(check-AV (library-name-without-version 'oops))
(check-AV (library-name-without-version '(foo "oops")))
;; library-name-version
(check (library-name-version '(foo zab (0 4))) => '(0 4))
(check (library-name-version '(foo zab ())) => '())
(check (library-name-version '(foo zab)) => #F)
(check-AV (library-name-version 'oops))
(check-AV (library-name-version '(foo "oops")))


(check-report)
