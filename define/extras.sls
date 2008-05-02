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
    case-lambda/AV lambda/AV (rename (lambda/AV λ/AV)) define/AV
    case-lambda/? lambda/? (rename (lambda/? λ/?)) define/?)
  (import 
    (rnrs)
    (for (only (xitomatl macro-utils) formals-ok? syntax->list) expand)
    (only (xitomatl common-unstandard) format)
    (xitomatl conditions))
  
  (define (define-values-error expected received-vals)
    (apply assertion-violation 'define-values
      (format "expected ~a values, received ~a values" expected (length received-vals))
      received-vals))
  
  (define-syntax my:define-values
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id* ...) expr)
         (formals-ok? #'(id* ...) stx)  ;; prevents duplicates
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
               (define id* 
                 (let ([v t*]) (set! t* #f) v)) 
               ...))])))
  

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
  
  (define-syntax case-lambda/AV--meta
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt name [frmls . body] ...)
         (with-syntax ([AV (datum->syntax #'ctxt 'AV)])
           #'(let ([AV (make-AV 'name)])
               (case-lambda [frmls . body] ...)))])))
  
  (define-syntax case-lambda/AV
    (lambda (stx)
      (syntax-case stx ()
        [(kw [frmls body0 body* ...] ...)
         #'(case-lambda/AV--meta kw "some <case-lambda/AV>" [frmls body0 body* ...] ...)])))
  
  (define-syntax lambda/AV
    (lambda (stx)
      (syntax-case stx ()
        [(kw frmls body0 body* ...)
         #'(case-lambda/AV--meta kw "some <lambda/AV>" [frmls body0 body* ...])])))
  
  (define-syntax define/AV
    (lambda (stx)
      (syntax-case stx ()
        [(kw (fname . frmls) body0 body* ...)
         #'(define fname
             (case-lambda/AV--meta kw fname [frmls body0 body* ...]))])))
  
  (define (make-arg-check-failed who)
    (lambda (pred-form arg-name arg-value)
      (assertion-violation/conditions who "argument check failed" (list arg-value)
        (make-argument-name-condition arg-name) 
        (make-predicate-condition pred-form))))
  
  (define-syntax case-lambda/?--meta
    (lambda (stx)
      (define (frml-id frml)
        (syntax-case frml () [(id pred) #'id] [id #'id]))
      (syntax-case stx ()
        [(_ fname [frmls . body] ...)
         (with-syntax ([((frmls* ... . #(frmlR)) ...) 
                        (map (lambda (f)
                               (syntax-case f ()
                                 [(f* ...) #'(f* ... . #(()))]
                                 [(f* ... . #(r)) (identifier? #'r) f]
                                 [(f* ... . #(r p)) #'(f* ... . #([r p]))]))
                             #'(frmls ...))])
           (with-syntax ([((id* ... idR) ...)
                          (map (lambda (f) (map frml-id (syntax->list f)))
                               #'((frmls* ... frmlR) ...))]
                         [(([cid* pred*] ...) ...) 
                          (map 
                            (lambda (f) 
                              (remp (lambda (x) (or (identifier? x)
                                                    (null? (syntax->datum x))))
                                    (syntax->list f))) 
                            #'((frmls* ... frmlR) ...))])
             #'(let ([acf (make-arg-check-failed 'fname)])
                 (case-lambda 
                   [(id* ... . idR)
                    (unless (pred* cid*) (acf 'pred* 'cid* cid*))
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
    (syntax-rules ()
      [(_ (fname . frmls) body0 body* ...)
       (define fname
         (case-lambda/?--meta fname [frmls body0 body* ...]))]))
)
