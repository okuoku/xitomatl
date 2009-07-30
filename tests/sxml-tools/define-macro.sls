#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl tests sxml-tools define-macro)
  (export
    define-macro
    gensym)
  (import
    (rnrs)
    (only (xitomatl common) gensym))
  
  (define-syntax define-macro
    (syntax-rules ()
      ((_ (name . args) . body)
       (define-syntax name
         (lambda (stx)
           (define T 
             (case-lambda
               (args . body)
               (oops (syntax-violation #F "invalid syntax" stx))))
           (syntax-case stx ()
             ((ctxt form (... ...))
              (datum->syntax #'ctxt
                (apply T (syntax->datum #'(form (... ...))))))))))))
)
