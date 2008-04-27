;;; Provides define-values, define which does procedure currying,
;;; and define-syntax which does procedure style.  Also, /AV and /?
;;; lambda and define for convenient argument checking.
#!r6rs
(library (xitomatl define extras)
  (export 
    (rename
      (my:define-values define-values)
      (my:define define)
      (my:define-syntax define-syntax))
    lambda/AV define/AV
    lambda/? define/?)
  (import 
    (rnrs)
    (for (only (xitomatl macro-utils) unique-ids?/raise) expand)
    (only (xitomatl common-unstandard) format)
    (xitomatl conditions))
  
  (define (define-values-error expected received-vals)
    (apply assertion-violation 'define-values
      (format "expected ~a values, received ~a values" expected (length received-vals))
      received-vals))
  
  (define-syntax my:define-values
    ;; NOTE: When Bug #162785 is fixed, the t*s won't be necessary,
    ;;       and id*s can be defined in front of dummy and set! from the case-lambda.
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id* ...) expr)
         (and (for-all identifier? #'(id* ...)) 
              (unique-ids?/raise #'(id* ...) stx))
         (with-syntax ([(t* ...) (generate-temporaries #'(id* ...))])
           #`(begin
               (define t*) ...
               (define dummy 
                 (call-with-values 
                  (lambda () #f expr) ;; #f first to prevent internal defines
                  (case-lambda
                    [(id* ...)
                     (set! t* id*) ...
                     #f]
                    [otherwise 
                     (define-values-error #,(length #'(id* ...)) otherwise)])))
               (define id* t*) ...))])))
  

  (define-syntax my:define
    (syntax-rules ()
      [(_ ((maybe-list . f1) . f2) expr expr* ...)
       (my:define (maybe-list . f1)
         (lambda f2 expr expr* ...))]
      [(_ . rest)
       (define . rest)]))

    
  (define-syntax my:define-syntax
    (syntax-rules ()
      [(_ (name . args) expr expr* ...)
       (define-syntax name
         (lambda args expr expr* ...))]
      [(_ . rest)
       (define-syntax . rest)]))
  
  (define-syntax lambda/AV--meta
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt name frmls body0 body* ...)
         (with-syntax ([AV (datum->syntax #'ctxt 'AV)])
           #'(lambda frmls
               (let ([AV (lambda (msg . irrts) 
                           (apply assertion-violation 'name msg irrts))])
                 body0 body* ...)))])))
  
  (define-syntax lambda/AV
    (lambda (stx)
      (syntax-case stx ()
        [(kw frmls body0 body* ...)
         #'(lambda/AV--meta kw <lambda> frmls body0 body* ...)])))
  
  (define-syntax define/AV
    (lambda (stx)
      (syntax-case stx ()
        [(kw (fname . frmls) body0 body* ...)
         #'(define fname
             (lambda/AV--meta kw fname frmls body0 body* ...))])))
  
  (define (argument-check-failed who pred arg-name arg-value)
    (assertion-violation/conditions who "argument check failed" (list arg-value)
      (make-argument-name-condition arg-name) 
      (make-predicate-condition pred)))
  
  (define-syntax lambda/?--meta
    ;;; NOTE: remember, frmlN is not checked
    (lambda (stx)
      (define (frml-id frml)
        (syntax-case frml () [(id pred) #'id] [id #'id]))
      (syntax-case stx ()
        [(_ fname (frmls* ... . frmlN) body0 body* ...)
         (and (identifier? #'fname)
              (for-all identifier? (map frml-id #'(frmls* ...)))
              (or (null? (syntax->datum #'frmlN))
                  (identifier? #'frmlN)))
         (with-syntax ([(id* ...) (map frml-id #'(frmls* ...))]
                       [([cid* pred*] ...) (remp identifier? #'(frmls* ...))])
           #'(lambda (id* ... . frmlN)
               (unless (pred* cid*)
                 (argument-check-failed 'fname 'pred* 'cid* cid*))
               ...
               (let () body0 body* ...)))])))
  
  (define-syntax lambda/?
    (syntax-rules ()
      [(_ frmls body0 body* ...)
       (lambda/?--meta <lambda> frmls body0 body* ...)]))
  
  (define-syntax define/?
    (syntax-rules ()
      [(_ (fname . frmls) body0 body* ...)
       (define fname
         (lambda/?--meta fname frmls body0 body* ...))]))
)
