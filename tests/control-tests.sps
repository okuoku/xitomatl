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
  (rnrs eval)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl control))

(define-syntax check-values
  (syntax-rules (=>)
    ((_ expr => vals ...)
     (check (let-values ((v expr)) v) => (list vals ...)))))

(define-syntax check-SV
  (syntax-rules ()
    [(_ expr)
     (check (catch ex ([else (syntax-violation? ex)])
              (eval 'expr (environment '(rnrs) '(xitomatl control)))
              'unexpected-return)
            => #T)]))

;;;; begin0

(let ([x 1] [n 0])
  (check (begin0 
           (begin (set! n (+ 1 n))
                  (- x))
           (set! x (+ 1 x))
           (set! x (* x x))) 
         => -1)
  (check x => 4)
  (check n => 1))
(check-SV (begin0))

;;;; compose

(define (add1 x) (+ 1 x))

(check-values ((compose))
              => )
(check-values ((compose) 1)
              => 1)
(check-values ((compose) 1 2 3)
              => 1 2 3)
(check-values ((compose -) 2)
              => -2)
(check-values ((compose -) 9 8 7)
              => -6)
(check-values ((compose / - add1) 2)
              => -1/3)
(check-values ((compose add1 - /) 2)
              => 1/2)
(check-values ((compose (lambda args (apply values (cddr (map - args))))
                        (lambda args (apply values (reverse args))))
               1 2 3 4 5)
              => -3 -2 -1)
(check-values ((compose string->number
                        list->string
                        vector->list
                        vector
                        (lambda () (values #\1 #\2))))
              => 12)


(check-report)
