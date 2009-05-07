;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl sxml-match (1 0))
  (export
    sxml-match
    sxml-match-let
    sxml-match-let*)
  (import
    (rename (rnrs) 
            (syntax->datum syntax-object->datum)
            (syntax-violation raise-syntax-error))
    (xitomatl include)
    (for (xitomatl sxml-match void) run expand))
  
  (define-syntax module
    (syntax-rules ()
      [(_ _ _ . r) (begin . r)]))
  
  (define-syntax provide
    (syntax-rules ()
      [(_ . _) (begin)]))
  
  (define-syntax require
    (syntax-rules ()
      [(_ . _) (begin)]))

  (define-syntax let/ec
    (syntax-rules ()
      [(_ n . b)
       (call/cc (lambda (n) . b))]))
  
  (include/resolve ("xitomatl" "sxml-match") "sxml-match.ss")
)
