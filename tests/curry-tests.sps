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
  (xitomatl curry)
  (xitomatl srfi lightweight-testing))

(define-syntax check-values
  (syntax-rules ()
    [(_ expr => vals ...)
     (check (let-values ([v expr]) v) => (list vals ...))]))

(define list-4-2
  ((curry list 3) 4 2))
(check (list-4-2 5) => '(4 2 5))
(check (list-4-2 5 6) => '(4 2 5 6))
(check ((((curry vector 3) 2) 4) 5) => '#(2 4 5))

(define/curry (c1 x) (- x))
(check (c1 42) => -42)
(check ((((c1))) 33) => -33)

(define/curry (c2 x y) (- y x))
(define c2a (c2 11))
(check (c2a 1) => -10)
(check (c2a 20) => 9)
(define c2b (c2 7))
(check (c2b 1) => -6)
(check (c2b 20) => 13)
(check (c2 55 23) => -32)

(define/curry (c3 x y z . r)
  (values r z y x))
(check-values (c3 1 2 3 4 5 6)
              => '(4 5 6) 3 2 1)
(define c3a (c3 1 2))
(check-values (c3a 3) => '() 3 2 1)
(check-values (c3a 3 4 5 6 7) => '(4 5 6 7) 3 2 1)
(define c3b (c3 1))
(check-values ((c3b 2) 3) => '() 3 2 1)
(check-values (c3b 2 3) => '() 3 2 1)
(check-values ((c3b 2) 3 4 5) => '(4 5) 3 2 1)

(define/curry (c4 a b c d e f g) (vector a b c d e f g))
(check ((((((((c4) 1) 2) 3) 4) 5) 6) 7) => '#(1 2 3 4 5 6 7))
(check (((((c4 1 2) 3 4) 5) 6) 7) => '#(1 2 3 4 5 6 7))
(check ((((c4 1 2) 3 4 5) 6) 7) => '#(1 2 3 4 5 6 7))
(check ((((c4 1) 2) 3 4 5 6) 7) => '#(1 2 3 4 5 6 7))
(check (c4 1 2 3 4 5 6 7) => '#(1 2 3 4 5 6 7))
(check ((c4 1 2 3 4) 5 6 7) => '#(1 2 3 4 5 6 7))

(check-report)
