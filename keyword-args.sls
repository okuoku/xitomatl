#!r6rs
(library (xitomatl keyword-args)
  (export
    #;lambda/kw/e
    #;lambda/kw/r
    define/kw/e
    define/kw/r
    #;:-)
  (import
    (rnrs)
    (xitomatl conditions)
    #;(xitomatl keyword-args multi-phase))
  
  ;; TODO: prevent duplicate keyword arguments when no kw-alist specified
  
  
  
  #;(define-syntax :-
    (lambda (stx)
      (syntax-case stx ()
        [(_ name value)
         (identifier? #'name)
         #'(make-kw-arg (cons 'name value))])))
  
  (define (check-kw-args incoming input-arg-names dflt-names has-kw-rest who)
    (and 
      ;; Check for missing required keyword arguments
      (let ([no-dflts (remp (lambda (ian) 
                              (memp (lambda (dn) (symbol=? ian dn)) 
                                    dflt-names)) 
                            input-arg-names)])
        (for-all (lambda (nd)
                   (or (member nd incoming)
                       (assertion-violation/conditions who 
                         "missing required keyword argument" '()
                         (make-argument-name-condition nd))))
                 no-dflts))
      ;; Check for unknown keyword arguments, if no kw-rest
      (or has-kw-rest
          (for-all (lambda (kw)
                     (or (member kw input-arg-names)
                         (assertion-violation who "unknown keyword argument" kw)))
                   incoming))))
  
  #;(define-syntax lambda/kw--meta
    (lambda (stx)
      (syntax-case stx ()      
        [(_ who orig-stx eval-time-or-run-time
            (kw-arg* ... . kw-rest) body* ...)
         (and (positive? (length #'(body* ...)))
              (or (positive? (length #'(kw-arg* ...))) (identifier? #'kw-rest))
              (or (null? (syntax->datum #'kw-rest)) (identifier? #'kw-rest))
              (unique-ids?/raise  
                (append
                  (map
                    (lambda (ka)
                      (syntax-case ka ()
                        [kw (identifier? #'kw) #'kw]
                        [(kw default-expr) (identifier? #'kw) #'kw]
                        [else (syntax-violation #f "invalid keyword argument" #'orig-stx ka)]))
                    #'(kw-arg* ...))
                  (if (identifier? #'kw-rest) (list #'kw-rest) '()))
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
                          (if (identifier? #'kw-rest) #'(kw-rest) (generate-temporaries '(t)))])
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
        [(_ name eval-time-or-run-time (kw-arg* ... . kw-rest) body* ...)
         (with-syntax ([(input-arg-name* ...) 
                        (map (lambda (ka)
                               (syntax-case ka () [(kw de) #'kw] [kw #'kw]))
                             #'(kw-arg* ...))]
                       [input-kw-rest 
                        (if (identifier? #'kw-rest) #'(kw-rest) #'())])
           (with-syntax ([(apply-arg-name* ...)
                          (generate-temporaries #'(input-arg-name* ...))]
                         [apply-kw-rest 
                          (generate-temporaries #'input-kw-rest)])
             (with-syntax ([([etime-dflt* dflt-temp* dflt-name* dflt-expr*] ...) 
                            (filter values
                                    (map (lambda (ka)
                                           (syntax-case ka () 
                                             [(kw de) 
                                              (list (car (generate-temporaries #'(kw)))
                                                    (cdr (assp (lambda (ian)
                                                                 (bound-identifier=? #'kw ian))
                                                               (map cons 
                                                                    #'(input-arg-name* ...)
                                                                    #'(apply-arg-name* ...))))
                                                    #'kw
                                                    #'de)]
                                             [else #f]))
                                         #'(kw-arg* ...)))])
               (with-syntax ([def-etime-dflts
                               (case (syntax->datum #'eval-time-or-run-time) 
                                 [(eval-time) #'((define etime-dflt* dflt-expr*) ...)]
                                 [(run-time) #'()])]
                             [(dflt-val-expr* ...)
                              (case (syntax->datum #'eval-time-or-run-time) 
                                [(eval-time) #'(etime-dflt* ...)]
                                [(run-time) #'(dflt-expr* ...)])])
                 #'(begin
                     #;(define first-class                  
                     ;; the lambda/kw--meta will: 
                     ;;   at expand time check syntax of (kw-arg* ... . kw-rest) body* ...
                     ;;   and at run-time (when first-class called) process+check the args
                     #;(lambda/kw--meta name orig-stx eval-time-or-run-time (kw-arg* ... . kw-rest)
                     (the-proc arg-name* ... . ka)))
                     (define (the-proc input-arg-name* ... . input-kw-rest) 
                       body* ...)
                     (define-syntax name
                       (make-variable-transformer
                         (lambda (stx)
                           (syntax-case stx (set! :-)
                             [(set! _ val)         ;;; set! pattern
                              (syntax-violation #f "can not set! a define/kw binding" stx)]
                             #;[kw                   ;;; reference pattern
                             (identifier? #'kw) 
                             #'first-class]
                             [(kw)         ;; so it gets checked
                              #'(kw [:-])]
                             [(_ [:- [kw* val-expr*] (... ...)])         ;;;; call pattern
                              (and (for-all identifier? #'(kw* (... ...)))
                                   (check-kw-args (syntax->datum #'(kw* (... ...)))
                                                  '(input-arg-name* ...)
                                                  '(dflt-name* ...)
                                                  (not (null? 'apply-kw-rest))
                                                  'name)                                   
                                   #|;; Check for missing required keyword arguments
                                   (let ([no-dflts (remp (lambda (ian) 
                                                           (memp (lambda (dn) (symbol=? ian dn)) 
                                                                 '(dflt-name* ...))) 
                                                         '(input-arg-name* ...))])
                                     (for-all (lambda (nd)
                                                (or (member nd (syntax->datum #'(kw* (... ...))))
                                                    (assertion-violation/conditions 'name 
                                                      "missing required keyword argument" '()
                                                      (make-argument-name-condition nd))))
                                              no-dflts))
                                   ;; Check for unknown keyword arguments, if no kw-rest
                                   (or (not (null? 'apply-kw-rest))
                                       (for-all (lambda (kw)
                                                  (or (member kw '(input-arg-name* ...))
                                                      (assertion-violation/conditions 'name 
                                                        "unknown keyword argument" (list kw))))
                                                (syntax->datum #'(kw* (... ...)))))|#)
                              ;; Process keyword arguments at expand-time
                              (with-syntax ([(kwt* (... ...))
                                             (map (lambda (kw)
                                                    (cond [(assp (lambda (ian) (symbol=? kw ian))
                                                                 (list (cons 'input-arg-name*
                                                                             #'apply-arg-name*)
                                                                       ...))
                                                           => cdr]
                                                          [else 
                                                           (assert (not (null? 'apply-kw-rest)))
                                                           (car (generate-temporaries (list kw)))]))
                                                  (syntax->datum #'(kw* (... ...))))]
                                            [akan 
                                             (syntax-case #'apply-kw-rest ()
                                               [(id) #'id] [else #f])])
                                (with-syntax ([kw-rest-clause
                                               (if (identifier? #'akan) 
                                                 #`([akan (list (cons 'kw* kwt*) (... ...) 
                                                                (cons 'dflt-name* dflt-temp*) ...)])
                                                 #'())])
                                  #'(let ([dflt-temp* dflt-val-expr*]
                                          ...)
                                      (let ([kwt* val-expr*]
                                            (... ...))
                                        (let kw-rest-clause
                                          (the-proc apply-arg-name* ... . apply-kw-rest))))))]))))
                     . def-etime-dflts)))))])))
  
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
         #'(define/kw--meta name eval-time kw-formals body0 body* ...)])))
  
  (define-syntax define/kw/r
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) body0 body* ...)
         (identifier? #'name)
         #'(define/kw--meta name run-time kw-formals body0 body* ...)])))
  
)
