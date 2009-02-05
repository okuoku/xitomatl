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
(import
  (rnrs)
  (xitomatl vectors)
  (srfi :78 lightweight-testing))

(define-syntax check-invalid
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (and (assertion-violation? ex)
                                  (who-condition? ex)
                                  (message-condition? ex)
                                  (list (condition-who ex)
                                        (condition-message ex)))])
              expr
              'unexpected-return)
            => '(subvector "invalid indices"))]))

(check (subvector '#() 0 0) => '#())
(check-invalid (subvector '#() 0 1))
(check-invalid (subvector '#() 1 0))
(check-invalid (subvector '#() 1 1))
(check (subvector '#(1) 0 0) => '#())
(check (subvector '#(a) 0 1) => '#(a))
(check (subvector '#(a) 1 1) => '#())
(check-invalid (subvector '#(1) 0 2))
(check-invalid (subvector '#(1) 1 0))
(check-invalid (subvector '#(1) 55 55))
(check (subvector '#(a b) 0 0) => '#())
(check (subvector '#(a b) 0 1) => '#(a))
(check (subvector '#(a b) 1 2) => '#(b))
(check (subvector '#(a b) 0 2) => '#(a b))
(check (subvector '#(a b) 2 2) => '#())
(check-invalid (subvector '#(a b) 3 3))
(check-invalid (subvector '#(a b) 1 0))
(check-invalid (subvector '#(a b) 2 3))
(check (subvector '#(a b c d e f) 0 0) => '#())
(check (subvector '#(a b c d e f) 0 1) => '#(a))
(check (subvector '#(a b c d e f) 1 2) => '#(b))
(check (subvector '#(a b c d e f) 0 2) => '#(a b))
(check (subvector '#(a b c d e f) 2 2) => '#())
(check (subvector '#(a b c d e f) 0 5) => '#(a b c d e))
(check (subvector '#(a b c d e f) 1 5) => '#(b c d e))
(check (subvector '#(a b c d e f) 4 5) => '#(e))
(check (subvector '#(a b c d e f) 5 6) => '#(f))
(check (subvector '#(a b c d e f) 3 6) => '#(d e f))
(check (subvector '#(a b c d e f) 2 4) => '#(c d))
(check (subvector '#(a b c d e f) 6 6) => '#())
(check-invalid (subvector '#(a b c d e f) 5 4))
(check-invalid (subvector '#(a b c d e f) 1 7))
(check-invalid (subvector '#(a b c d e f) 42 45))


(check-report)
