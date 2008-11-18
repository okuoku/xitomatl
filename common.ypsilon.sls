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

(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time
    with-output-to-string
    ;; TODO: add to as needed/appropriate
    )
  (import
    (rnrs)
    (only (core) format gensym microsecond set-current-output-port!)
    (prefix (only (core) pretty-print) ypsilon:)
    (only (time) time))
  
  (define (add1 x) (+ x 1))

  (define (sub1 x) (- x 1))
  
  (define (fprintf port fmt-str . fmt-args)
    (put-string port (apply format fmt-str fmt-args)))
  
  (define (printf fmt-str . fmt-args)
    (apply fprintf (current-output-port) fmt-str fmt-args))
  
  (define pretty-print
    (case-lambda
      [(x)
       (pretty-print x (current-output-port))]
      [(x p)
       (ypsilon:pretty-print x p)
       (newline p)]))

  (define (with-output-to-string thunk)
    (let-values ([(sop get) (open-string-output-port)])
      (let ((temp #f))
        (dynamic-wind
         (lambda ()
           (set! temp (current-output-port))
           (set-current-output-port! sop))
         (lambda () (thunk))
         (lambda ()
           (set-current-output-port! temp)))
        (get))))
)
