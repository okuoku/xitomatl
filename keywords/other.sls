;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl keywords other)
  (export
    process-options
    parse-kw-formals
    missing-value--default
    missing-keyword--default
    predicate-false--default
    keyword-condition? 
    condition-keyword)
  (import
    (rnrs)
    (only (xitomatl macro-utils) identifier?/name=? formals-ok?/raise)
    (only (xitomatl conditions) make-predicate-expression-condition))
  
  (define (process-options full-stx kw-spec)
    (let loop ([options (with-syntax ([(_ . opts) kw-spec]) #'opts)] 
               [default #F] [predicate #F] [boolean #F])
      (syntax-case options ()
        [(:default expr . rest)
         (and (identifier?/name=? #':default ':default)
              (not default))
         (loop #'rest #'expr predicate boolean)]
        [(:predicate expr . rest)
         (and (identifier?/name=? #':predicate ':predicate)
              (not predicate))
         (loop #'rest default #'expr boolean)]
        [(:boolean . rest)
         (and (identifier?/name=? #':boolean ':boolean)
              (not boolean))
         (loop #'rest default predicate #T)]
        [()
         (not (and boolean (or default predicate)))
         (values default predicate boolean)]
        [_
         (syntax-violation #F "invalid options for keyword" full-stx kw-spec)])))  

  (define (parse-kw-formals full-stx kw-formals . default)
    (let parse ([kwf kw-formals] [pos-ids '()])
      (syntax-case kwf ()
        [([kw-id . opts] ... . additional-id)
         (let ([pos-ids (reverse pos-ids)])
           (and (formals-ok?/raise (append pos-ids #'(kw-id ... . additional-id)) full-stx)
                (cons* pos-ids
                       #'([kw-id . opts] ...)
                       (if (identifier? #'additional-id)
                         #'(additional-id)
                         default))))]
        [(pos . r)
         (parse #'r (cons #'pos pos-ids))])))
  
  (define-condition-type &keyword &condition
    make-keyword-condition keyword-condition?
    (keyword condition-keyword))

  (define (AV who msg kw-id . more)
    (raise
     (apply condition
            (make-assertion-violation)
            (make-who-condition who)
            (make-message-condition msg)
            (make-keyword-condition kw-id)
            more)))
  
  (define (missing-value--default who kw-id)
    (AV who "keyword missing value" kw-id))

  (define (missing-keyword--default who kw-id)
    (AV who "missing required keyword" kw-id))

  (define (predicate-false--default who kw-id pred-form value)
    (AV who "keyword predicate false" kw-id 
        (make-predicate-expression-condition pred-form)
        (make-irritants-condition (list value))))  
)
