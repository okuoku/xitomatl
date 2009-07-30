#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl fmt let-optionals*)
  (export
    let-optionals*)
  (import
    (rnrs base))

  (define-syntax let-optionals*
    (syntax-rules ()
      ((_ opt-ls () body ...)
       (let () body ...))
      ((_ (expr ...) vars body ...)
       (let ((tmp (expr ...)))
         (let-optionals* tmp vars body ...)))
      ((_ tmp ((var default) . rest) body ...)
       (let ((var (if (pair? tmp) (car tmp) default))
             (tmp2 (if (pair? tmp) (cdr tmp) '())))
         (let-optionals* tmp2 rest body ...)))
      ((_ tmp tail body ...)
       (let ((tail tmp))
         body ...))))
)
