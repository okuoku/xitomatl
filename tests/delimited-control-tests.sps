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
  (xitomatl delimited-control)
  (srfi :78 lightweight-testing))

;------------------------------------------------------------------------
;			Shift tests

(check (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))) 
       => 117)

(check (* 10 (reset (* 2 (shift g (reset
                                   (* 5 (shift f (+ (f 1) 1))))))))
       => 60)

(check (let ((f (lambda (x) (shift k (k (k x))))))
           (+ 1 (reset (+ 10 (f 100)))))
       => 121)

(check (reset
        (let ((x (shift f (cons 'a (f '())))))
          (shift g x)))
       => '(a))

(define (p x) (if (eq? x p) '(p p) `(p ,x)))
(define (shift* p) (shift f (p f)))
(check (reset (let ([x 'abcde]) (eq? x ((shift* shift*) x))))
       => #t)

(define (traverse/shift xs)
  (letrec ([visit
            (lambda (xs)
              (if (null? xs)
                '()
                (visit (shift k (cons (car xs) (k (cdr xs)))))))])
    (reset (visit xs))))

(check (traverse/shift '(1 2 3 4 5))
       => '(1 2 3 4 5))

;------------------------------------------------------------------------
;			Control tests
; Example from Sitaram, Felleisen

(check (let ([g (prompt (* 2 (control k k)))])
         (* 3 (prompt (* 5 (abort (g 7))))))
       => 42)

(define (traverse/control xs)
  (letrec ([visit
            (lambda (xs)
              (if (null? xs)
                '()
                (visit (control k (cons (car xs) (k (cdr xs)))))))])
    (prompt (visit xs))))

(check (traverse/control '(1 2 3 4 5))
       => '(5 4 3 2 1))

(check (+ 10 (prompt (+ 2 (control k (+ 100 (k (k 3)))))))
       => 117)

(check (prompt (let ((x (control f (cons 'a (f '()))))) (control g x)))
       => '())

(check (prompt ((lambda (x) (control l 2)) (control l (+ 1 (l 0)))))
       => 2)

(check (prompt (control f (cons 'a (f '()))))
       => '(a))

(check (prompt (let ((x (control f (cons 'a (f '()))))) (control g (g x))))
       => '(a))

(check (prompt (let ((x 'abcde)) (eq? x ((control k (control k2 (k k2))) x))))
       => #t)

;------------------------------------------------------------------------
;			control0 and shift0 tests

(check (+ 10 (prompt0 (+ 2 (control0 k (+ 100 (k (k 3)))))))
       => 117)

(check (prompt0 (prompt0
                 (let ((x (control0 f (cons 'a (f '()))))) (control0 g x))))
       => '())

(check (+ 10 (prompt0 (+ 2 (shift0 k (+ 100 (k (k 3)))))))
       => 117)

(check (prompt0 (cons 'a (prompt0 (shift0 f (shift0 g '())))))
       => '())


(check-report)
