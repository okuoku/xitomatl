#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl vectors)
  (export
    subvector)
  (import
    (rnrs)
    (only (xitomatl define) define/?/AV)
    (only (xitomatl predicates) exact-integer?))
  
  (define/?/AV (subvector (v vector?) (start exact-integer?) (end exact-integer?)) 
    (let ((len (vector-length v)))
      (unless (<= 0 start end len)
        (AV "invalid indices" start end v))
      (let ((n (make-vector (- end start))))
        (let loop ((x 0)
                   (y start))
          (cond ((= y end)
                 n)
                (else
                 (vector-set! n x (vector-ref v y))
                 (loop (+ 1 x) (+ 1 y))))))))

)
