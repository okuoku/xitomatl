;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl ssax private-5-1 error)
  (export
    make-errorer)
  (import
    (rnrs))

  (define (make-errorer who)
    (lambda (msg . more)
      (error who
             (call-with-string-output-port
               (lambda (sop)
                 (for-each (lambda (x) (display x sop))
                           (cons msg more)))))))
)
