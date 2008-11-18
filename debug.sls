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
(library (xitomatl debug)
  (export
    dprint dprint-mark
    eprint eprint-mark
    format printf fprintf pretty-print
    print-exception print-condition)
  (import
    (rnrs)
    (xitomatl srfi parameters)
    (only (xitomatl common) format printf fprintf pretty-print)
    (only (xitomatl exceptions) print-exception reraise)
    (only (xitomatl conditions) print-condition))
  
  (define dprint-mark (make-parameter "***"))
  
  (define-syntax dprint
    (lambda (stx)
      (syntax-case stx ()
        [(_ expr ...)
         (positive? (length #'(expr ...)))
         #'(let ([cep (current-error-port)]
                 [mark (dprint-mark)])
             (fprintf cep "~a\n" mark)           
             (let-values ([vs expr])
               (pretty-print 'expr cep)  ;; does newline
               (display "=>\n" cep)
               (for-each (lambda (v) (pretty-print v cep)) 
                         vs)
               (fprintf cep "~a\n" mark)           
               (apply values vs))
             ...)])))
  
  (define eprint-mark (make-parameter "***"))
  
  (define (print-exn exn)
    (let ([cep (current-error-port)]
          [mark (eprint-mark)])
      (fprintf cep "~a\n" mark)
      (print-exception exn cep)
      (fprintf cep "~a\n" mark)
      (reraise exn)))
  
  (define-syntax eprint
    (lambda (stx)
      (syntax-case stx ()
        [(_ expr ...)
         (positive? (length #'(expr ...)))
         #'(begin
             (let-values ([vs (with-exception-handler
                                print-exn
                                (lambda () #f expr))])
               (apply values vs))
             ...)])))
  
)
