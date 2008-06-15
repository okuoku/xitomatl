;;; Provides define-values, define which does procedure currying,
;;; and define-syntax which does procedure style.  Also, /AV and /?
;;; lambda and define for convenient argument checking.
#!r6rs
(library (xitomatl define extras)
  (export 
    (rename
      (my:define define)
      (my:define-syntax define-syntax))
    define-values
    case-lambda/AV lambda/AV (rename (lambda/AV λ/AV)) define/AV
    case-lambda/? lambda/? (rename (lambda/? λ/?)) define/?
    case-lambda/?/AV lambda/?/AV (rename (lambda/?/AV λ/?/AV)) define/?/AV)
  (import 
    (rnrs)
    (xitomatl define define-values)
    (for (only (xitomatl macro-utils) formals-ok? syntax->list) expand)
    (only (xitomatl common-unstandard) format)
    (xitomatl conditions))
  
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
  
  (define (make-AV who)
    (lambda (msg . irrts) 
      (apply assertion-violation who msg irrts)))
  
  (define-syntax AV-wrap
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt name expr)
         (with-syntax ([AV (datum->syntax #'ctxt 'AV)])
           #'(let ([AV (make-AV 'name)])
               #f  ;; prevent internal defines in expr 
               expr))])))
  
  (define-syntax case-lambda/AV--meta
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt name [frmls . body] ...)
         #'(AV-wrap ctxt name (case-lambda [frmls . body] ...))])))
  
  (define-syntax case-lambda/AV
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt [frmls body0 body* ...] ...)
         #'(case-lambda/AV--meta ctxt "some <case-lambda/AV>" [frmls body0 body* ...] ...)])))
  
  (define-syntax lambda/AV
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt frmls body0 body* ...)
         #'(case-lambda/AV--meta ctxt "some <lambda/AV>" [frmls body0 body* ...])])))
  
  (define-syntax define/AV
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt (fname . frmls) body0 body* ...)
         (identifier? #'fname)
         #'(define fname
             (case-lambda/AV--meta ctxt fname [frmls body0 body* ...]))]
        [(ctxt name expr)
         (identifier? #'name)
         #'(define name
             (AV-wrap ctxt name 
               expr))])))
  
  (define (make-arg-check-failed who)
    (lambda (pred-form arg-name arg-value)
      (assertion-violation/conditions who "argument check failed" (list arg-value)
        (make-argument-name-condition arg-name) 
        (make-predicate-condition pred-form))))
  
  (define-syntax case-lambda/?--meta
    (lambda (stx)
      (define (frml-id frml)
        (syntax-case frml () [(id pred) #'id] [_ frml]))
      (define (needs-check? frml)
        (syntax-case frml () [(id pred) #t] [_ #f]))
      (syntax-case stx ()
        [(_ fname [frmls . body] ...)
         (with-syntax ([((f ... fr) ...) 
                        (map (lambda (f)
                               (syntax-case f ()
                                 [(f ... . #(r p)) #'(f ... (r p))]
                                 [(f ... . r) #'(f ... r)]))
                             #'(frmls ...))])
           (with-syntax ([((id ... idr) ...)
                          (map (lambda (fl) (map frml-id (syntax->list fl)))
                               #'((f ... fr) ...))]
                         [(((cid p) ...) ...) 
                          (map (lambda (fl) (filter needs-check? (syntax->list fl))) 
                               #'((f ... fr) ...))])
             #'(let ([acf (make-arg-check-failed 'fname)])
                 (case-lambda 
                   [(id ... . idr)
                    (unless (p cid) (acf 'p 'cid cid))
                    ...
                    (let () . body)]
                   ...))))])))
  
  (define-syntax case-lambda/?
    (syntax-rules ()
      [(_ [frmls body0 body* ...] ...)
       (case-lambda/?--meta "some <case-lambda/?>" [frmls body0 body* ...] ...)]))
  
  (define-syntax lambda/?
    (syntax-rules ()
      [(_ frmls body0 body* ...)
       (case-lambda/?--meta "some <lambda/?>" [frmls body0 body* ...])]))
  
  (define-syntax define/?
    (lambda (stx)
      (syntax-case stx ()
        [(_ (fname . frmls) body0 body* ...)
         (identifier? #'fname)
         #'(define fname
             (case-lambda/?--meta fname [frmls body0 body* ...]))]
        [(_ name expr) 
         (identifier? #'name)
         (with-syntax ([CL/? (datum->syntax #'name 'case-lambda/?)]
                       [L/? (datum->syntax #'name 'lambda/?)])
           #'(define name
               (let ()
                 (define-syntax CL/? 
                   (syntax-rules ()
                     [(_ [frmls b0 b* (... ...)] (... ...))
                      (case-lambda/?--meta name [frmls b0 b* (... ...)] (... ...))]))
                 (define-syntax L/?
                   (syntax-rules ()
                     [(_ frmls b0 b* (... ...))
                      (case-lambda/?--meta name [frmls b0 b* (... ...)])]))
                 #f  ;; prevent internal defines in expr
                 expr)))])))
  
  (define-syntax case-lambda/?/AV
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt [frmls body0 body* ...] ...)
         (with-syntax ([name "some <case-lambda/?/AV>"])
           #'(AV-wrap ctxt name
               (case-lambda/?--meta name [frmls body0 body* ...] ...)))])))
  
  (define-syntax lambda/?/AV
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt frmls body0 body* ...)
         (with-syntax ([name "some <lambda/?/AV>"])
           #'(AV-wrap ctxt name
               (case-lambda/?--meta name [frmls body0 body* ...])))])))
  
  (define-syntax define/?/AV
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt (fname . frmls) body0 body* ...)
         (identifier? #'fname)
         #'(define fname
             (AV-wrap ctxt fname
               (case-lambda/?--meta fname [frmls body0 body* ...])))]
        [(_ name expr)
         (identifier? #'name)
         #'(define/? name
             (AV-wrap name name
               expr))])))
)
