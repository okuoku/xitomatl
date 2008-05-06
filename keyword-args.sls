#!r6rs
(library (xitomatl keyword-args)
  (export
    lambda/kw
    lambda/kw-r
    define/kw
    define/kw-r
    :-)
  (import
    (rnrs)
    (for (only (xitomatl indexes) enumerate) expand)
    (for (xitomatl macro-utils) expand)
    (for (xitomatl keyword-args macro-helpers) run expand))
  
  (define-record-type kw-args-grp (fields kw-alist))  
  
  (define-syntax :-
    (lambda (stx)
      (syntax-case stx ()
        [(_ [name* expr*] ...)
         (for-all identifier? #'(name* ...))
         #'(make-kw-args-grp (list (cons 'name* expr*) ...))])))
  
  (define (not-kw-args-grp who arg0 . arg*)
    (apply assertion-violation who "not a keyword arguments group" arg0 arg*))
  
  (define (process-args args who default-vals known has-kw-rest)
    (unless (kw-args-grp? args)
      (not-kw-args-grp who args))
    (let loop ([args (kw-args-grp-kw-alist args)] [vals default-vals])
      (if (null? args)
        vals
        (let ([arg (car args)]) 
          (if (or has-kw-rest (memq (car arg) known))
            (loop (cdr args) (cons arg vals))
            (unknown who (car arg)))))))
  
  (define (get-kw-val who arg-name vals)
    (if (null? vals)
      (missing who arg-name)
      (let ([kw.val (car vals)])
        (if (eq? arg-name (car kw.val))
          (cdr kw.val)
          (get-kw-val who arg-name (cdr vals))))))
  
  (define-syntax lambda/kw--meta
    (lambda (stx)
      (syntax-case stx ()      
        [(_ who eval-time-or-run-time (kw-arg* ... . kw-rest) body* ...)
         (with-syntax ([(arg-name* ...) 
                        (kw-arg*->ids #'(kw-arg* ...))]
                       [defaults-expr 
                         (with-syntax ([([kw de] ...) (filter (lambda (ka)
                                                                (syntax-case ka ()
                                                                  [(kw de) #t] [_ #f])) 
                                                              #'(kw-arg* ...))])
                           #'(list (cons 'kw de) ...))]
                       [ret-kw-alist 
                        (if (identifier? #'kw-rest) #'kw-rest (gen-temp))]
                       [has-kw-rest
                        (identifier? #'kw-rest)])             
           (case (syntax->datum #'eval-time-or-run-time)
             [(eval-time)
              #'(let ([kw-alist defaults-expr])
                  (define kw-lambda
                    (case-lambda 
                      [(args)
                       (let ([ret-kw-alist 
                              (process-args args 'who kw-alist '(arg-name* ...) has-kw-rest)])
                         (let ([arg-name* (get-kw-val 'who 'arg-name* ret-kw-alist)]
                               ...)
                           body* ...))]
                      [() 
                       (kw-lambda [:-])]
                      [oops
                       (apply not-kw-args-grp 'who oops)]))
                  kw-lambda)]
             [(run-time)
              #'(letrec ([kw-lambda
                          (case-lambda 
                            [(args)
                             (let ([ret-kw-alist 
                                    (process-args args 'who defaults-expr '(arg-name* ...) 
                                                  has-kw-rest)])
                               (let ([arg-name* (get-kw-val 'who 'arg-name* ret-kw-alist)]
                                     ...)
                                 body* ...))]
                            [() 
                             (kw-lambda [:-])]
                            [oops
                             (apply not-kw-args-grp 'who oops)])])
                  kw-lambda)]))])))
    
  (define-syntax define/kw--meta
    (lambda (stx)
      (syntax-case stx ()
        [(_ name eval-time-or-run-time (kw-arg* ... . kw-rest) body* ...)
         (with-syntax* ([(input-arg-name* ...) 
                         (kw-arg*->ids #'(kw-arg* ...))]
                        [([etime-dflt* dt-idx* input-dflt-name* dflt-expr*] ...) 
                         (filter values
                                 (map (lambda (ka idx t)
                                        (syntax-case ka () 
                                          [(kw de) (list t idx #'kw #'de)]
                                          [_ #f]))
                                      #'(kw-arg* ...)
                                      (enumerate #'(input-arg-name* ...))
                                      (syntax->list (generate-temporaries #'(input-arg-name* ...)))))]
                        [((def-etime-dflts ...) dflt-val-expr* ...)
                         (case (syntax->datum #'eval-time-or-run-time) 
                           [(eval-time) #'(((define etime-dflt* dflt-expr*) ...) etime-dflt* ...)]
                           [(run-time) #'(() dflt-expr* ...)])])
           #`(begin
               def-etime-dflts ...
               (define (the-proc input-arg-name* ... . kw-rest) 
                 body* ...)
               (define first-class                  
                 ;; The lambda/kw--meta will at run-time (when first-class called) 
                 ;; check and process the kw-args
                 (lambda/kw--meta name eval-time-or-run-time (kw-arg* ... . kw-rest)
                   #,(if (identifier? #'kw-rest)
                       #'(apply the-proc input-arg-name* ... kw-rest)
                       #'(the-proc input-arg-name* ...))))
               (define-syntax name
                 (make-variable-transformer
                  (lambda (stx)
                    (syntax-case stx (set! :-)
                      [(set! id val)
                       (syntax-violation #f 
                         "can not set! a define/kw or define/kw-r binding (lame?)" stx #'id)]
                      [ref                  ;; reference pattern
                       (identifier? #'ref) 
                       #'first-class]
                      [(x)                  ;; call pattern
                       #'(x [:-])]
                      [(_ [:- [kw* val-expr*] (... ...)])         ;; call pattern
                       (and (for-all identifier? #'(kw* (... ...)))
                            (check-kw-args (syntax->datum #'(kw* (... ...)))
                                           '(input-arg-name* ...)
                                           '(input-dflt-name* ...)
                                           #,(identifier? #'kw-rest)
                                           'name))
                       ;; Process keyword arguments at expand-time
                       (with-syntax* ([(apply-arg-name* (... ...))
                                       (generate-temporaries '(input-arg-name* ...))]
                                      [(kwt* (... ...))
                                       (let f ([l (map (lambda (kw)
                                                         (cond [(assp (lambda (ian) (symbol=? kw ian))
                                                                      (map cons
                                                                        '(input-arg-name* ...)
                                                                        #'(apply-arg-name* (... ...))))
                                                                => cdr]
                                                               [else  ;; kw is unknown
                                                                (assert (identifier? #'kw-rest))
                                                                ;; Give this unknown kw-arg a
                                                                ;; new temp id so that it will
                                                                ;; be put in kw-rest
                                                                (gen-temp)]))
                                                       (syntax->datum #'(kw* (... ...))))]
                                               [a '()])
                                         (if (null? l) (reverse a)
                                           ;; Give dup kw-args a new temp id, so that the last
                                           ;; one is captured and passed to the-proc as the 
                                           ;; "official" one, and so all the dups can be put 
                                           ;; in kw-rest
                                           (f (cdr l)
                                              (cons (let ([x (car l)])
                                                      (if (memp (lambda (y) (free-identifier=? x y))
                                                                (cdr l))
                                                        (gen-temp)
                                                        x)) 
                                                    a))))]
                                     [(kw*.kwt*-r (... ...))
                                      ;; so last dup is returned by assoc/assv/assq on kw-rest
                                      (reverse #'((cons 'kw* kwt*) (... ...)))]
                                     [(dflt-temp* (... ...))
                                      (map (lambda (i) (list-ref #'(apply-arg-name* (... ...)) i))
                                           '(dt-idx* ...))]
                                     [(dve* (... ...))
                                      #'(dflt-val-expr* ...)]
                                     [(dt-kwr* (... ...))  
                                      ;; used when kw-rest expression needs to capture defaults
                                      (generate-temporaries #'(dflt-temp* (... ...)))]
                                     [(idn* (... ...))
                                      #'(input-dflt-name* ...)])
                         #'(let ([dflt-temp* dve*]
                                 (... ...))
                             (let ([dt-kwr* dflt-temp*] 
                                   (... ...))
                               (let ([kwt* val-expr*]  ;; these shadow dflt-temp*
                                     (... ...))
                                 ;; apply-arg-name* captures kwt* and/or dflt-temp*
                                 (the-proc apply-arg-name* (... ...)
                                           #,@(if (identifier? #'kw-rest)
                                                #'(kw*.kwt*-r (... ...) 
                                                   (cons 'idn* dt-kwr*) (... ...))
                                                '()))))))]
                      [(_ any0 any* (... ...))        ;; call pattern
                       #'(not-kw-args-grp 'name any0 any* (... ...))]))))))])))
  
  (define-syntax lambda/kw
    (lambda (stx)
      (syntax-case stx ()
        [(_ kw-formals body0 body* ...)
         (check-kw-formals #'kw-formals stx)
         #'(lambda/kw--meta "some <lambda/kw>" eval-time
             kw-formals body0 body* ...)])))
  
  (define-syntax lambda/kw-r
    (lambda (stx)
      (syntax-case stx ()
        [(_ kw-formals body0 body* ...)
         (check-kw-formals #'kw-formals stx)
         #'(lambda/kw--meta "some <lambda/kw-r>" run-time
             kw-formals body0 body* ...)])))
  
  (define-syntax define/kw
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) body0 body* ...)
         (and (identifier? #'name)
              (check-kw-formals #'kw-formals stx))
         #'(define/kw--meta name eval-time kw-formals body0 body* ...)])))
  
  (define-syntax define/kw-r
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) body0 body* ...)
         (and (identifier? #'name)
              (check-kw-formals #'kw-formals stx))
         #'(define/kw--meta name run-time kw-formals body0 body* ...)])))
  
)
