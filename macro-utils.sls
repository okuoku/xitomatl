#!r6rs
(library (xitomatl macro-utils)
  (export
    gensym
    duplicate-id unique-ids? unique-ids?/raise)
  (import
    (rnrs)
    (only (xitomatl common-unstandard) gensym))
  
  (define (duplicate-id ls)
    (if (null? ls)
      #f
      (or
        (let loop ([x (car ls)] [rest (cdr ls)])
          (if (null? rest)
            #f
            (if (bound-identifier=? x (car rest))
              x
              (loop x (cdr rest)))))
        (duplicate-id (cdr ls)))))
  
  (define (unique-ids? ls)
    (not (duplicate-id ls)))
  
  (define (unique-ids?/raise ls stx-form)
    (define dup (duplicate-id ls))
    (if dup
      (syntax-violation #f "duplicate identifier" stx-form dup)
      #t))
)
