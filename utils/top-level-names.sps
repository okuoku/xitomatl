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
(import
  (rnrs)
  (xitomatl ports)
  (xitomatl match))

(define top-level-names
  (case-lambda
    [(input)
     (top-level-names input "define" "define-syntax")]
    [(input . look-for)
     (define (match/print look-for)
       (match-lambda
         [('library _ ('export . _) ('import . _) body ...)
          (for-each (match/print look-for) body)]
         [(f (n . r) . b) 
          (and (symbol? f) (memq f look-for) (symbol? n))
          (begin (display n) (newline))]
         [(f n . r) 
          (and (symbol? f) (memq f look-for) (symbol? n))
          (begin (display n) (newline))]
         [_ #f]))
     (let ([look-for (map string->symbol look-for)])
       (for-each (match/print look-for) (call-with-input-file input read-all)))]
    [args
     (assertion-violation (car (command-line)) 
       "invalid command line arguments" args)]))

(apply top-level-names (cdr (command-line)))
