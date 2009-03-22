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
  (xitomatl records)
  (srfi :78 lightweight-testing))

(define-record-type A (fields a (mutable b) c))
(define-record-type B (parent A) (fields (mutable d) e))
(define-record-type C (parent B))
(define-record-type D (parent C) (fields f (mutable g) (mutable h)))
(define a (make-A 1 2 3))
(define b (make-B 1 2 3 4 5))
(define c (make-C 1 2 3 4 5))
(define d (make-D 1 2 3 4 5 6 7 8))
(check (record-type-fields (record-rtd a)) => '(a b c))
(check (record-type-fields (record-rtd b)) => '(a b c d e))
(check (record-type-fields (record-rtd c)) => '(a b c d e))
(check (record-type-fields (record-rtd d)) => '(a b c d e f g h))
(define A-accessors (record-type-accessors (record-rtd a)))
(define B-accessors (record-type-accessors (record-rtd b)))
(define C-accessors (record-type-accessors (record-rtd c)))
(define D-accessors (record-type-accessors (record-rtd d)))
(check (length A-accessors) => 3)
(check (for-all procedure? A-accessors) => #t)
(check (length B-accessors) => 5)
(check (for-all procedure? B-accessors) => #t)
(check (length C-accessors) => 5)
(check (for-all procedure? C-accessors) => #t)
(check (length D-accessors) => 8)
(check (for-all procedure? D-accessors) => #t)
(check (map (lambda (get) (get a)) A-accessors) => '(1 2 3))
(check (map (lambda (get) (get b)) B-accessors) => '(1 2 3 4 5))
(check (map (lambda (get) (get c)) C-accessors) => '(1 2 3 4 5))
(check (map (lambda (get) (get d)) D-accessors) => '(1 2 3 4 5 6 7 8))
(define A-mutators (record-type-mutators (record-rtd a)))
(define B-mutators (record-type-mutators (record-rtd b)))
(define C-mutators (record-type-mutators (record-rtd c)))
(define D-mutators (record-type-mutators (record-rtd d)))
(check (length A-mutators) => 3)
(check (length B-mutators) => 5)
(check (length C-mutators) => 5)
(check (length D-mutators) => 8)
(let ([what (lambda (m) 
              (cond [(procedure? m) 'p] 
                    [(not m) #f]
                    [else 'bad]))])
  (check (map what A-mutators) => '(#f p #f))
  (check (map what B-mutators) => '(#f p #f p #f))
  (check (map what C-mutators) => '(#f p #f p #f))
  (check (map what D-mutators) => '(#f p #f p #f #f p p)))
(for-each (lambda (setter!) (setter! a 'new)) (filter values A-mutators))
(check (map (lambda (get) (get a)) A-accessors) => '(1 new 3))
(for-each (lambda (setter!) (setter! b 'new)) (filter values B-mutators))
(check (map (lambda (get) (get b)) B-accessors) => '(1 new 3 new 5))
(for-each (lambda (setter!) (setter! c 'new)) (filter values C-mutators))
(check (map (lambda (get) (get c)) C-accessors) => '(1 new 3 new 5))
(for-each (lambda (setter!) (setter! d 'new)) (filter values D-mutators))
(check (map (lambda (get) (get d)) D-accessors) => '(1 new 3 new 5 6 new new))


(check-report)
