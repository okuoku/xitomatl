(library (xitomatl keyword-args)
  (export
    #;lambda/kw/e
    #;lambda/kw/r
    define/kw/e
    define/kw/r
    :-)
  (import
    (xitomatl define traced)
    (rename (rnrs) (define r:define) (lambda r:lambda) (define-syntax r:define-syntax))
    (only (xitomatl macro-utils) unique-ids?/raise)
    (xitomatl keyword-args multi-phase))
  
  ;; TODO: prevent duplicate keyword arguments when no kw-alist specified
  
  
  
  (r:define-syntax :-
    (r:lambda (stx)
      (syntax-case stx ()
        [(_ name value)
         (identifier? #'name)
         #'(make-kw-arg (cons 'name value))])))  
  
  #;(define-syntax lambda/kw--meta
    (lambda (stx)
      (syntax-case stx ()      
        [(_ who orig-stx eval-time-or-run-time
            (kw-arg* ... . kw-all) body* ...)
         (and (positive? (length #'(body* ...)))
              (or (positive? (length #'(kw-arg* ...))) (identifier? #'kw-all))
              (or (null? (syntax->datum #'kw-all)) (identifier? #'kw-all))
              (unique-ids?/raise  
                (append
                  (map
                    (lambda (ka)
                      (syntax-case ka ()
                        [kw (identifier? #'kw) #'kw]
                        [(kw default-expr) (identifier? #'kw) #'kw]
                        [else (syntax-violation #f "invalid keyword argument" #'orig-stx ka)]))
                    #'(kw-arg* ...))
                  (if (identifier? #'kw-all) (list #'kw-all) '()))
                #'orig-stx))
         (let ([info (map (lambda (ka)
                            (syntax-case ka ()
                              [kw (identifier? #'kw) #'kw]
                              [(kw default-expr) (identifier? #'kw) (cons #'kw #'default-expr)])) 
                          #'(kw-arg* ...))])
           (with-syntax ([(arg-name* ...) (map (lambda (i) (if (pair? i) (car i) i)) info)]
                         [defaults-expr 
                          (with-syntax ([((kw . dflt-expr) ...) (filter pair? info)])
                            #'(list (cons 'kw dflt-expr) ...))]
                         [(ret-kw-alist) 
                          (if (identifier? #'kw-all) #'(kw-all) (generate-temporaries '(t)))])
             (syntax-case #'eval-time-or-run-time (eval-time run-time)
               [eval-time
                #'(let ([kw-alist defaults-expr])
                    (lambda args
                      (let ([ret-kw-alist (process-args args 'who kw-alist)])
                        (let ([arg-name* (get-kw-val 'who 'arg-name* ret-kw-alist)]
                              ...)
                          body* ...))))]
               [run-time
                #'(lambda args
                    (let ([ret-kw-alist (process-args args 'who defaults-expr)])
                      (let ([arg-name* (get-kw-val 'who 'arg-name* ret-kw-alist)]
                            ...)
                        body* ...)))])))]      
        [(_ _ orig-stx . _)
         (syntax-violation #f "invalid syntax" #'orig-stx)])))
  
  (define-syntax define/kw--meta
    (lambda (stx)
      (syntax-case stx ()
        [(_ name orig-stx eval-time-or-run-time
                 (kw-arg* ... . kw-all) body* ...)
         (with-syntax ([(arg-name* ...) 
                        (map (lambda (ka)
                               (syntax-case ka ()
                                 [kw (identifier? #'kw) #'kw]
                                 [(kw default-expr) (identifier? #'kw) #'kw]))
                             #'(kw-arg* ...))]
                       [ka 
                        (if (identifier? #'kw-all) #'(kw-all) #'())]                       
                       [([etime-dflt* dflt-arg-name*/outer dflt-expr*] ...) 
                        (filter values
                                (map (lambda (ka) 
                                       (syntax-case ka () 
                                         [(kw default-expr) 
                                          (with-syntax ([(et) (generate-temporaries #'(kw))])
                                            #'(et kw default-expr))]
                                         [else #f]))
                                     #'(kw-arg* ...)))])
           (with-syntax ([def-etime-dflts
                          (case (syntax->datum #'eval-time-or-run-time) 
                            [(eval-time) #'((define etime-dflt* dflt-expr*) ...)]
                            [(run-time) #'()])]
                         [(dflt-arg-val-expr*/outer ...)
                          (case (syntax->datum #'eval-time-or-run-time) 
                            [(eval-time) #'(etime-dflt* ...)]
                            [(run-time) #'(dflt-expr* ...)])])
             #'(begin
                 #;(define first-class                  
                   ;; the lambda/kw--meta will: 
                   ;;   at expand time check syntax of (kw-arg* ... . kw-all) body* ...
                   ;;   and at run-time (when first-class called) process+check the args
                   #;(lambda/kw--meta name orig-stx eval-time-or-run-time (kw-arg* ... . kw-all)
                   (client-proc arg-name* ... . ka)))
                 (define (client-proc arg-name* ... . ka) 
                   body* ...)
                 (r:define-syntax name
                   (make-variable-transformer
                    (lambda (stx)
                      (syntax-case stx (set! :-)
                        [(set! _ val)         ;;; set! pattern
                         (syntax-violation #f "can not set! a define/kw binding" stx)]
                        #;[kw                   ;;; reference pattern
                         (identifier? #'kw) 
                         #'first-class]
                        [(ctxt [:- kw* val-expr*] (... ...))         ;;;; call pattern
                         (for-all identifier? #'(kw* (... ...)))
                         ;; Handle keyword arguments at expand-time
                         ;; TODO: check for missing keyword arguments
                         (with-syntax ([apply-kw-all-name 
                                        (datum->syntax #'ctxt 'kw-all)]
                                       [(dat* (... ...))
                                        (generate-temporaries '(dflt-arg-name*/outer ...))]
                                       [(dflt-arg-name* (... ...)) 
                                        (map (lambda (an) (datum->syntax #'ctxt an))
                                             '(dflt-arg-name*/outer ...))]
                                       [(dflt-arg-val-expr* (... ...)) 
                                        #'(dflt-arg-val-expr*/outer ...)])
                           (with-syntax ([kw-all-clause
                                          (if (symbol? 'kw-all) 
                                            #`([apply-kw-all-name
                                                (list #,@(reverse #'((cons 'kw* kw*) (... ...))) 
                                                      (cons 'dflt-arg-name* dat*) (... ...))])
                                            #'())]
                                         [(apply-arg-name* (... ...))
                                          (append (map (lambda (an) (datum->syntax #'ctxt an))
                                                       '(arg-name* ...))
                                                  (if (symbol? 'kw-all) 
                                                    (list #'apply-kw-all-name)
                                                    '()))])
                             #'(let ([dat* dflt-arg-val-expr*]
                                     (... ...))
                                 (let ([dflt-arg-name* dat*]
                                       (... ...))
                                   (let ([kw* val-expr*]
                                         (... ...))
                                     (let kw-all-clause
                                       (client-proc apply-arg-name* (... ...))))))))]))))
                 . def-etime-dflts)))])))
  
  #;(define-syntax lambda/kw/e
    (lambda (stx)
      (syntax-case stx ()
        [(who kw-formals body0 body* ...)
         #`(lambda/kw--meta who #,stx eval-time
             kw-formals body0 body* ...)])))
  
  #;(define-syntax lambda/kw/r
    (lambda (stx)
      (syntax-case stx ()
        [(who kw-formals body0 body* ...)
         #`(lambda/kw--meta who #,stx run-time
             kw-formals body0 body* ...)])))
  
  (define-syntax define/kw/e
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) body0 body* ...)
         (identifier? #'name)
         #`(define/kw--meta name #,stx eval-time
             kw-formals body0 body* ...)])))
  
  (define-syntax define/kw/r
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) body0 body* ...)
         (identifier? #'name)
         #`(define/kw--meta name #,stx run-time
             kw-formals body0 body* ...)])))
  
)
