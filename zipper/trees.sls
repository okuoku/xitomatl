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
(library (xitomatl zipper trees)
  (export 
    zip-iterate/df 
    zip-to-nth/df
    ;; most of (xitomatl zipper base)
    zipper? zipper-thing zipper-cont
    :zip-keep-val:
    zip-finish 
    zip-n)
  (import
    (rnrs)
    (xitomatl zipper base)
    (only (xitomatl lists) map/left-right/preserving)
    (only (xitomatl define) define/?)
    (only (xitomatl predicates) exact-non-negative-integer?))
  
  
  (define (depth-first proc tree)
    (let ([x (proc tree)])
      (cond
        [(not (eq? x :zip-keep-val:)) x]
        ; the node was not handled -- descend
        [(null? tree) '()]
        [(not (pair? tree)) tree] ; an atom
        [else
         (let ([t (map/left-right/preserving (lambda (kid) (depth-first proc kid)) 
                                             tree)])
           (if (eq? t tree) tree t))])))
  
  (define _zip-iterate/df (make-zip-iterator depth-first))
  (define/? (zip-iterate/df [l list?]) (_zip-iterate/df l))
  
  (define/? (zip-to-nth/df [l list?] [n exact-non-negative-integer?]) 
    (zip-n (_zip-iterate/df l) n))  
)
