#!r6rs
(library (xitomatl define)
  (export 
    define-values
    define/who
    define/AV
    define/?
    define/?/AV)
  (import 
    (rnrs)
    (xitomatl define define-values)
    (for (only (xitomatl macro-utils) formals-ok? syntax->list) expand)
    (only (xitomatl common) format)
    (only (xitomatl exceptions) assertion-violation/conditions)
    (xitomatl conditions))
  
  (define-syntax who-wrap
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt name expr)
         (with-syntax ([who (datum->syntax #'ctxt 'who)])
           #'(let ([who 'name])
               #f  ;; prevent internal defines in expr 
               expr))])))
  
  (define-syntax define/who
    (lambda (stx)
      (syntax-case stx ()
        [(kw (fname . frmls) b0 b ...)
         (identifier? #'fname)
         #'(kw fname
             (lambda frmls b0 b ...))]
        [(ctxt name expr)
         (identifier? #'name)
         #'(define name
             (who-wrap ctxt name 
               expr))])))
  
  
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
  
  (define-syntax define/AV
    (lambda (stx)
      (syntax-case stx ()
        [(kw (fname . frmls) b0 b ...)
         (identifier? #'fname)
         #'(kw fname
             (lambda frmls b0 b ...))]
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
               (let-syntax ([CL/? (syntax-rules ()
                                    [(_ . r) (case-lambda/?--meta name . r)])]
                            [L/? (syntax-rules ()
                                   [(_ . r) (case-lambda/?--meta name r)])])
                 expr)))])))
  
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
