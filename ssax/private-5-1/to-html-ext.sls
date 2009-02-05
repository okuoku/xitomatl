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
(library (xitomatl ssax private-5-1 to-html-ext)
  (export
    make-header
    make-navbar
    make-footer
    universal-conversion-rules
    universal-protected-rules
    alist-conv-rules
    find-Header
    generic-web-rules)
  (import
    (except (rnrs) error)
    (rnrs r5rs)
    (xitomatl include)
    (except (srfi :13 strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash)
    (only (xitomatl file-system base) file-regular?)
    (xitomatl ssax private-5-1 to-html)
    (xitomatl ssax tree-trans)
    (xitomatl ssax private-5-1 error)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 util))
  
  (define (OS:file-length filename)
    (if (file-regular? filename) 1 0))
  
  (define error (make-errorer "(xitomatl ssax private-5-1 to-html-ext)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SXML-to-HTML-ext.scm")
)
