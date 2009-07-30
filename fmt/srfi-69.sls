#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl fmt srfi-69)
  (export
    make-eq?-table
    hash-table-ref/default
    hash-table-set!
    hash-table-walk)
  (import
    (rnrs base)
    (rnrs hashtables))

  (define make-eq?-table make-eq-hashtable)
  
  (define hash-table-ref/default hashtable-ref)

  (define hash-table-set! hashtable-set!)

  (define (hash-table-walk ht proc)
    (let-values (((keys vals) (hashtable-entries ht)))
      (vector-for-each proc keys vals)))  
)
