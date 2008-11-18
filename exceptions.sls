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
(library (xitomatl exceptions)
  (export
    catch reraise
    warning warning/conditions assertion-violation/conditions error/conditions
    print-exception)
  (import
    (rnrs)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand)
    (only (xitomatl conditions) print-condition)
    (only (xitomatl common) pretty-print))
  
  (define-syntax catch
    (lambda (stx)
      (syntax-case stx ()
        [(_ var (in-clause ...) expr0 expr ...)
         (with-syntax* 
             ([catch-k (gen-temp)]
              [(out-clause ...) 
               (map (lambda (ic)
                      (syntax-case ic (=>)
                        [(test => proc) 
                         #'(test => (lambda (t) (catch-k (lambda () (proc t)))))]
                        [(test)
                         #'(test => (lambda (t) (catch-k (lambda () t))))]
                        [(test/else expr ...)
                         #'(test/else (catch-k (lambda () expr ...)))]))
                    #'(in-clause ...))])
           #`((call/cc
                (lambda (catch-k)
                  (lambda ()
                    (with-exception-handler
                      (lambda (var)
                        #,(and (positive? (length #'(out-clause ...)))
                               #'(cond out-clause ...))
                        (reraise var))
                      (lambda ()
                        expr0 expr ...)))))))])))
  
  (define (reraise obj)
    ;; If R7RS makes exceptions discernable as continuable or not,
    ;; this will change to use raise or raise-continuable depending.
    (raise-continuable obj))
  
  (define (make-raiser/conditions raise-it make-main-condition)
    (lambda (who msg irrts . cndts)
      (raise-it 
       (apply condition
              (make-main-condition)
              (if who 
                (make-who-condition who)
                (condition))
              (make-message-condition msg)
              (make-irritants-condition irrts)
              cndts))))
  
  (define assertion-violation/conditions
    (make-raiser/conditions raise make-assertion-violation))
  
  (define error/conditions
    (make-raiser/conditions raise make-error))
  
  (define warning/conditions
    (make-raiser/conditions raise-continuable make-warning))
  
  (define (warning who msg . irrts)
    (warning/conditions who msg irrts))
  
  (define print-exception 
    (case-lambda
      [(exn)
       (print-exception exn (current-output-port))]
      [(exn p)
       (display "Exception:\n" p)
       (if (condition? exn)
         (print-condition exn p)
         (pretty-print exn p))]))
)
