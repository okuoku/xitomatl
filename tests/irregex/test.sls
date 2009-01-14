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
(library (xitomatl tests irregex test)
  (export
    test test-assert test-error test-group
    test-begin test-end test-exit)
  (import
    (rnrs)
    (srfi :78 lightweight-testing))
  
  (define-syntax test 
    (syntax-rules ()
      [(_ name expected expr)
       (test expected expr)]
      [(_ expected expr)
       (check expr => expected)]))
  
  (define-syntax test-assert
    (syntax-rules ()
      [(_ name expr)
       (test-assert expr)]
      [(_ expr)
       (check (and expr #t) => #t)]))
  
  (define-syntax test-error
    (syntax-rules ()
      [(_ name expr)
       (test-error expr)]
      [(_ expr)
       (check (guard (ex [(or (error? ex)
                              (assertion-violation? ex)) 
                          #t]
                         [else `(dont-know: ,ex)])
                expr
                '(succeeded: expr))
              => #t)]))
  
  (define-syntax test-group
    (syntax-rules () 
      [(_ name expr0 expr ...)
       (begin expr0 expr ...)]))
  
  (define-syntax test-begin
    (syntax-rules ()
      [(_)
       (begin)]))
  
  (define (test-end) (check-report))
  
  (define (test-exit n) 
    (if (check-passed? n)
      (exit)
      (exit #f)))
  
)
