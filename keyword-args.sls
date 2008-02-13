(library (keyword-args)
  (export
    lambda/kw/e
    lambda/kw/r
    define/kw/e
    define/kw/r
    :-)
  (import
    (rnrs)
    (indexes)
    (unique-ids))
  
  (define-record-type kw-arg 
    (fields name value))
  
  (define-syntax :-
    (lambda (stx)
      (syntax-case stx ()
        [(_ name value)
         (identifier? #'name)
         #'(make-kw-arg 'name value)])))
  
  (define (process-args args who kw-indexes kw-vals kw-missing-val)
    ;; kw-indexes - ((keyword-symbol-name . kw-vals-index) ...)
    ;; kw-vals - vector of default values, for those with defaults,
    ;;           and vector of returned values.
    ;; kw-missing-val - vector of #f or symbol, indicating whether an argument
    ;;                  must be supplied a value.
    ;; All keywords must be known.
    ;; All arguments without defaults must be in `args'.
    ;; TODO: prevent duplicate keyword arguments?
    ;; TODO: variety of handling modes, like PLT keywords system?
    (for-each
      (lambda (arg)
        (unless (kw-arg? arg)
          (assertion-violation who "not a keyword argument" arg))
        (let ([idx (cond [(assoc (kw-arg-name arg) kw-indexes) => cdr]
                         [else (assertion-violation who "unknown keyword" (kw-arg-name arg))])])
          (vector-set! kw-vals idx (kw-arg-value arg))
          (vector-set! kw-missing-val idx #f)))
      args)
    (vector-for-each
      (lambda (m?)
        (when m? (assertion-violation who "required keyword argument missing" m?)))
      kw-missing-val)
    kw-vals)
  
  (define-syntax lambda/kw--meta
    (lambda (stx)    
      (define (kw-arg*->syntaxes kw-arg*)
        (let ([specs
               (map
                 (lambda (ka i)
                   (syntax-case ka ()
                     [(kw default-expr)
                      (list #'kw i #'default-expr)]
                     [kw
                      (list #'kw i #f)]))
                 kw-arg*
                 (enumerate-indexes kw-arg*))])
          (list 
           ;; kw-indexes
           (map (lambda (s) (cons (car s) (cadr s))) specs)
           ;; kw-vals-expr
           #`(vector #,@(map (lambda (s) (caddr s)) specs))
           ;; kw-missing-val-expr
           #`(vector #,@(map (lambda (s) (if (caddr s) #f #`(quote #,(car s)))) specs)))))
      (syntax-case stx ()      
        [(_ who orig-stx eval-time-or-run-time
            (kw-arg* ...) body* ...)
         (and (positive? (length #'(body* ...)))
              (positive? (length #'(kw-arg* ...)))
              (unique-ids?/raise  
                (map
                  (lambda (ka)
                    (syntax-case ka ()
                      [kw (identifier? #'kw) #'kw]
                      [(kw default-expr) (identifier? #'kw) #'kw]
                      [else (syntax-violation #f "invalid keyword argument" #'orig-stx ka)]))
                  #'(kw-arg* ...))
                #'orig-stx))
         (with-syntax ([(kw-indexes kw-vals-expr kw-missing-val-expr) 
                        (kw-arg*->syntaxes #'(kw-arg* ...))])
           (with-syntax ([((arg-name* . idx*) ...)
                          #'kw-indexes])
             (syntax-case #'eval-time-or-run-time (eval-time run-time)
               [eval-time
                #'(let ([kw-vals kw-vals-expr])
                    (lambda args
                      (let ([ret-kw-vals
                             (process-args args 'who 'kw-indexes kw-vals kw-missing-val-expr)])
                        (let ([arg-name* 
                               (vector-ref ret-kw-vals idx*)]
                              ...)
                          body* ...))))]
               [run-time
                #'(lambda args
                    (let ([ret-kw-vals
                           (process-args args 'who 'kw-indexes kw-vals-expr kw-missing-val-expr)])
                      (let ([arg-name* 
                             (vector-ref ret-kw-vals idx*)]
                            ...)
                        body* ...)))])))]      
        [(_ _ orig-stx . _)
         (syntax-violation #f "invalid syntax" #'orig-stx)])))
  
  (define-syntax lambda/kw/e
    (lambda (stx)
      (syntax-case stx ()
        [(who formals body0 body* ...)
         #`(lambda/kw--meta who #,stx eval-time
             formals body0 body* ...)])))
  
  (define-syntax lambda/kw/r
    (lambda (stx)
      (syntax-case stx ()
        [(who formals body0 body* ...)
         #`(lambda/kw--meta who #,stx run-time
             formals body0 body* ...)])))
  
  (define-syntax define/kw/e
    (lambda (stx)
      (syntax-case stx ()
        [(_ (who . formals) body0 body* ...)
         (identifier? #'who)
         #`(define who
             (lambda/kw--meta who #,stx eval-time
               formals body0 body* ...))])))
  
  (define-syntax define/kw/r
    (lambda (stx)
      (syntax-case stx ()
        [(_ (who . formals) body0 body* ...)
         (identifier? #'who)
         #`(define who
             (lambda/kw--meta who #,stx run-time
               formals body0 body* ...))])))
  
)
