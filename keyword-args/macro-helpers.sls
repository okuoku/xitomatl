#!r6rs
(library (xitomatl keyword-args macro-helpers)
  (export
    check-kw-formals check-kw-args
    kw-arg*->ids
    missing unknown)
  (import
    (rnrs)
    (xitomatl conditions)
    (xitomatl macro-utils))
  
  (define (kw-arg*->ids kw-arg*)
    (map (lambda (ka)
           (syntax-case ka () 
             [(kw de) #'kw]
             [kw #'kw]))
         kw-arg*))
  
  (define (check-kw-formals kw-frmls orig-stx)
    (formals-ok? 
      (syntax-case kw-frmls ()
        [(kw-arg* ... . kw-rest)
         (with-syntax ([(id* ...) (kw-arg*->ids #'(kw-arg* ...))])
           #'(id* ... . kw-rest))])
      orig-stx))
  
  (define (missing who arg-name)
    (assertion-violation/conditions who 
      "missing required keyword argument" '()
      (make-argument-name-condition arg-name)))
      
  (define (unknown who arg-name)
    (assertion-violation who "unknown keyword argument" arg-name))
  
  (define (check-kw-args incoming input-arg-names dflt-names has-kw-rest who)
    (and
      ;; Check for unknown keyword arguments, if no kw-rest
      (or has-kw-rest
          (for-all (lambda (kw)
                     (or (member kw input-arg-names) (unknown who kw)))
                   incoming)) 
      ;; Check for missing required keyword arguments
      (let ([no-dflts (remp (lambda (ian)
                              (memp (lambda (dn) (symbol=? ian dn)) 
                                    dflt-names)) 
                            input-arg-names)])
        (for-all (lambda (nd)
                   (or (member nd incoming) (missing who nd)))
                 no-dflts))))
)
