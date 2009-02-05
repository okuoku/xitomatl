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
  (only (rnrs r5rs) quotient)
  (xitomatl irregex)
  (xitomatl match)
  (only (xitomatl strings) string-split string-intersperse)
  (only (xitomatl common) format)
  (only (xitomatl ports) port-for-each)
  (xitomatl tests irregex test)
  (xitomatl include))

(define (warning msg . irrts)
  (raise (condition (make-warning) 
                    (make-message-condition msg)
                    (make-irritants-condition irrts))))

(define (call-with-output-string proc)
  (call-with-string-output-port proc))

(define (call-with-input-string str proc)
  (call-with-port (open-string-input-port str) proc))

(define (sprintf fmt-str . args)
  (define (convert x)
    (let loop ([l (string->list x)] [a '()])
      (if (null? l)
        (list->string (reverse a))
        (if (and (pair? (cdr l)) (char=? (car l) #\~))
          (loop (cddr l) (cons* (char-downcase (cadr l)) (car l) a))
          (loop (cdr l) (cons (car l) a))))))
  (apply format (convert fmt-str) args))

(define read-line get-line)

(include/resolve ("xitomatl" "tests" "irregex") "test-irregex.scm")

(test-exit 1800)
