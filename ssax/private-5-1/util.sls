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
(library (xitomatl ssax private-5-1 util)
  (export
    ;; Only the ones (xitomatl ssax ---) and (xitomatl sxml-tools ---) need
    string->integer
    string-split
    make-char-quotator
    substring?
    string-whitespace?)
  (import
    (except (rnrs) error)
    (rnrs mutable-pairs)
    (xitomatl include)
    (xitomatl ssax private-5-1 error)
    (xitomatl ssax private-5-1 misc)
    (except (xitomatl srfi strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash))
  
  (define error (make-errorer "(xitomatl ssax private-5-1 util)"))

  ; Test if a string is made of only whitespace
  ; An empty string is considered made of whitespace as well
  (define (string-whitespace? str)
    (let ((len (string-length str)))
      (cond
        ((zero? len) #t)
        ((= 1 len) (char-whitespace? (string-ref str 0)))
        ((= 2 len) (and (char-whitespace? (string-ref str 0))
                        (char-whitespace? (string-ref str 1))))
        (else
         (let loop ((i 0))
           (or (>= i len)
               (and (char-whitespace? (string-ref str i))
                    (loop (inc i)))))))))
  
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "util.scm")
)
