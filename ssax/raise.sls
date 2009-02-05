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
(library (xitomatl ssax raise)
  (export
    parser-error
    ssax:warn)
  (import
    (rnrs)
    (only (xitomatl conditions) make-port-position-condition))
  
  (define (make-f raise-it first who)
    (lambda (port msg . other-msg)
      (raise-it
       (condition
        first
        (make-who-condition who)
        (make-message-condition (call-with-string-output-port
                                 (lambda (sop)
                                   (for-each (lambda (x) (display x sop))
                                             (cons msg other-msg)))))
        (if (and (port? port) (port-has-port-position? port))
          (make-port-position-condition (port-position port))
          (condition))
        (make-irritants-condition (list port))))))
   
  (define parser-error
    (make-f raise (make-error) 'ssax:parser))
   
  (define ssax:warn
    ;; None of these condition types are &serious,
    ;; so a default exception handler should return (per the R6RS),
    ;; allowing the SSAX code which called this to continue.
    (make-f raise-continuable (make-warning) 'ssax))
  
)
