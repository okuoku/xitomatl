;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
