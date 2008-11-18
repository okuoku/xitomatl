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
  (xitomatl records)
  (xitomatl srfi lightweight-testing))

(define-record-type A (fields a (mutable b) c))
(define-record-type B (parent A) (fields (mutable d) e))
(define-record-type C (parent B) (fields f (mutable g) (mutable h)))
(define a (make-A 1 2 3))
(define b (make-B 1 2 3 4 5))
(define c (make-C 1 2 3 4 5 6 7 8))
(define A-accessors (record-type-accessors (record-rtd a)))
(define B-accessors (record-type-accessors (record-rtd b)))
(define C-accessors (record-type-accessors (record-rtd c)))
(check (vector-length A-accessors) => 3)
(check (for-all procedure? (vector->list A-accessors)) => #t)
(check (vector-length B-accessors) => 5)
(check (for-all procedure? (vector->list B-accessors)) => #t)
(check (vector-length C-accessors) => 8)
(check (for-all procedure? (vector->list C-accessors)) => #t)
(check (vector-map (lambda (get) (get a)) A-accessors) => '#(1 2 3))
(check (vector-map (lambda (get) (get b)) B-accessors) => '#(1 2 3 4 5))
(check (vector-map (lambda (get) (get c)) C-accessors) => '#(1 2 3 4 5 6 7 8))
(define A-mutators (record-type-mutators (record-rtd a)))
(define B-mutators (record-type-mutators (record-rtd b)))
(define C-mutators (record-type-mutators (record-rtd c)))
(check (vector-length A-mutators) => 3)
(check (vector-length B-mutators) => 5)
(check (vector-length C-mutators) => 8)
(let ([what (lambda (m) 
              (cond [(procedure? m) 'p] 
                    [(eqv? m #f) #f]
                    [else 'bad]))])
  (check (vector-map what A-mutators) => '#(#f p #f))
  (check (vector-map what B-mutators) => '#(#f p #f p #f))
  (check (vector-map what C-mutators) => '#(#f p #f p #f #f p p)))
(for-each (lambda (setter!) (setter! a 'new)) (filter values (vector->list A-mutators)))
(check (vector-map (lambda (get) (get a)) A-accessors) => '#(1 new 3))
(for-each (lambda (setter!) (setter! b 'new)) (filter values (vector->list B-mutators)))
(check (vector-map (lambda (get) (get b)) B-accessors) => '#(1 new 3 new 5))
(for-each (lambda (setter!) (setter! c 'new)) (filter values (vector->list C-mutators)))
(check (vector-map (lambda (get) (get c)) C-accessors) => '#(1 new 3 new 5 6 new new))


(check-report)
