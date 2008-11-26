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
  (only (xitomatl common) add1 time)
  (xitomatl keywords))

;;; TODO: consult with people about better/other ways to measure

(define-syntax bigloop
  (syntax-rules ()
    [(_ n body ...)
     (let loop ([i 0])
       (unless (= i n)
         body ...
         (loop (add1 i))))]))

(define N #e1e8)
(display "\nN = ") (display N) (newline)

(define (n1 a) 1)
(define (n2 a b) 1)
(define (n3 a b c) 1)

(define/kw (dekw1 [a]) 1)
(display "\nOne argument, define/kw expand-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n1 1)))
(time (bigloop N (dekw1 'a 1)))

(define/kw (dekw2 [a] [b]) 1)
(display "\nTwo arguments, define/kw expand-time processing:\n")
(display "================================================\n")
(time (bigloop N))
(time (bigloop N (n2 1 2)))
(time (bigloop N (dekw2 'b 2 'a 1)))

(define/kw (dekw3 [a] [b] [c]) 1)
(display "\nThree arguments, define/kw expand-time processing:\n")
(display "==================================================\n")
(time (bigloop N))
(time (bigloop N (n3 1 2 3)))
(time (bigloop N (dekw3 'b 2 'c 3 'a 1)))

(define lekw1 (lambda/kw ([a]) 1))
(display "\nOne argument, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n1 1)))
(time (bigloop N (lekw1 'a 1)))

(define lekw2 (lambda/kw ([a] [b]) 1))
(display "\nTwo arguments, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n2 1 2)))
(time (bigloop N (lekw2 'b 2 'a 1)))

(define lekw3 (lambda/kw ([a] [b] [c]) 1))
(display "\nThree arguments, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n3 1 2 3)))
(time (bigloop N (lekw3 'b 2 'c 3 'a 1)))
