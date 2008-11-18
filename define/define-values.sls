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
(library (xitomatl define define-values)
  (export
    define-values)
  (import
    (rnrs)
    (for (only (xitomatl macro-utils) formals-ok?) expand)
    (only (xitomatl common) format))
  
  (define (define-values-error expected received-vals)
    (apply assertion-violation 'define-values
      (format "expected ~a values, received ~a values" expected (length received-vals))
      received-vals))
  
  (define-syntax define-values
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id* ... . rid) expr)
         (formals-ok? #'(id* ... . rid) stx)         
         #`(begin
             (define t 
               (call-with-values 
                (lambda () #f expr) ;; #f first to prevent internal defines
                (case-lambda
                  [(id* ... . rid)
                   (list id* ... #,@(if (identifier? #'rid) (list #'rid) '()))]
                  [otherwise
                   (define-values-error #,(length #'(id* ...)) otherwise)])))
             (define id*
               (let ([v (car t)]) (set! t (cdr t)) v))
             ...
             #,@(if (identifier? #'rid)
                  (list #'(define rid
                            (let ([v (car t)]) (set! t (cdr t)) v)))
                  '()))])))  
  
)
