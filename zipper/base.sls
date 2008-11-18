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

; Taken from Oleg's
; http://okmij.org/ftp/Scheme/zipper-in-scheme.txt

#!r6rs
(library (xitomatl zipper base)
  (export
    zipper? zipper-thing zipper-cont
    :zip-keep-val:
    make-zip-iterator
    zip-finish 
    zip-n)
  (import 
    (rnrs)
    (only (xitomatl delimited-control) shift reset)
    (only (xitomatl define) define/? define/?/AV)
    (only (xitomatl predicates) exact-non-negative-integer?))
  
  
  (define-record-type zipper (fields thing cont))
  
  (define :zip-keep-val: (list #T)) ;; unique object
  
  (define/? (make-zip-iterator [iterate procedure?])
    (lambda (x)
      (reset (iterate (lambda (y) (shift sk (make-zipper y sk))) 
                      x))))
  
  (define/? (zip-finish [z zipper?])
    (let loop ([z z])
      (let ([x ((zipper-cont z) (zipper-thing z))])
        (if (zipper? x) (loop x) x))))
  
  (define/?/AV (zip-n [z zipper?] [n exact-non-negative-integer?])
    (do ([i 0 (+ 1 i)]
         [z z ((zipper-cont z) :zip-keep-val:)])
      [(and (= i n)
            (if (zipper? z) #t (AV "not enough elements")))
       z]))
)
