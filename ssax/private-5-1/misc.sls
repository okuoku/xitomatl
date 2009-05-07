;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl ssax private-5-1 misc)
  (export
    inc dec
    ascii->char ucscode->char
    char-return char-tab char-newline
    call-with-input-string
    with-output-to-string)
  (import
    (rnrs)
    (only (xitomatl common) with-output-to-string))
  
  (define (inc n) (+ n 1))  
  (define (dec n) (- n 1))
  
  (define ascii->char integer->char)
  (define ucscode->char ascii->char)
  (define char-return (ascii->char 13))
  (define char-tab (ascii->char 9))
  (define char-newline (ascii->char 10))
  
  (define (call-with-input-string str proc)
    (call-with-port (open-string-input-port str) proc))  
)
