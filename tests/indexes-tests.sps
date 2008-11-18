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
  (xitomatl srfi lightweight-testing)
  (xitomatl indexes))

(check (iota 0) => '())
(check (iota 1) => '(0))
(check (iota 2) => '(0 1))
(check (iota 10) => '(0 1 2 3 4 5 6 7 8 9))
(check (enumerate '()) => '())
(check (enumerate '(a)) => '(0))
(check (enumerate '(a b)) => '(0 1))
(check (enumerate '(a b c d e f g h i)) => '(0 1 2 3 4 5 6 7 8))
(check (enumerate '#()) => '())
(check (enumerate '#(a)) => '(0))
(check (enumerate '#(a b)) => '(0 1))
(check (enumerate '#(a b c d e f g h i)) => '(0 1 2 3 4 5 6 7 8))
(check (enumerate "") => '())
(check (enumerate "a") => '(0))
(check (enumerate "ab") => '(0 1))
(check (enumerate "abcdefghi") => '(0 1 2 3 4 5 6 7 8))
(check (enumerate '#vu8()) => '())
(check (enumerate '#vu8(255)) => '(0))
(check (enumerate '#vu8(255 255)) => '(0 1))
(check (enumerate '#vu8(255 255 255 255 255 255 255 255 255)) => '(0 1 2 3 4 5 6 7 8))


(check-report)
