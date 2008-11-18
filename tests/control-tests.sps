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
  (rnrs eval)
  (xitomatl srfi lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl control))

(define-syntax check-SV
  (syntax-rules ()
    [(_ expr)
     (check (catch ex ([else (syntax-violation? ex)])
              (eval 'expr (environment '(rnrs) '(xitomatl control)))
              'unexpected-return)
            => #T)]))

(check (aif x (+ 1 2) 
         (* x x) 
         (assert #F))
       => 9)
(check (aif x (string? 'sym) 
         (assert #F) 
         (list x))
       => '(#F))
(check (aif x number? (+ 1 2)
         (* x x) 
         (assert #F))
       => 9)
(check (aif x integer? (+ 1.1 2)
         (assert #F) 
         (- x))
       => -3.1)
(let ([a 0] [b 0] [c 0] [d 0])
  (check (aif x (begin (set! a (+ 1 a)) integer?) (begin (set! b (+ 1 b)) (+ 1.1 2))
              (begin (set! c (+ 1 c)) 'bad) 
              (begin (set! d (+ 1 d)) (- x)))
         
         => -3.1)
  (check (list a b c d) => '(1 1 0 1)))
(check-SV (aif "oops" 'foo 'true 'false))

(let ([x 1] [n 0])
  (check (begin0 
           (begin (set! n (+ 1 n))
                  (- x))
           (set! x (+ 1 x))
           (set! x (* x x))) 
         => -1)
  (check x => 4)
  (check n => 1))


(check-report)
