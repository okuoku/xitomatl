;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl control)
  (export
    begin0
    compose)
  (import 
    (rnrs))
  
  (define-syntax begin0
    (syntax-rules ()
      [(_ form0 form1 ...)
       (let ([result form0])
         (begin form1 ... result))]))

  (define (compose . procs)
    (lambda args
      (let loop ((procs (reverse procs)) (args args))
        (if (null? procs)
          (apply values args)
          (let-values ((vals (apply (car procs) args)))
            (loop (cdr procs) vals))))))
)
