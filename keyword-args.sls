#!r6rs
(library (xitomatl keyword-args)
  (export
    lambda/kw
    lambda/kw/r
    define/kw
    define/kw/r
    :-)
  (import
    (rnrs)
    (xitomatl conditions)
    (for (xitomatl keyword-args macro-helpers) expand))
  
  (define-record-type kw-arg (fields name.value))  
  
  (define-syntax :-
    (lambda (stx)
      (syntax-case stx ()
        [(_ [name* expr*] ...)
         (for-all identifier? #'(name* ...))
         #'(list (make-kw-arg (cons 'name* expr*)) ...)])))
  
  (define (missing who arg-name)
    (assertion-violation/conditions who 
      "missing required keyword argument" '()
      (make-argument-name-condition arg-name)))
  
  (define (unknown who arg-name)
    (assertion-violation who "unknown keyword argument" arg-name))
  
  (define (process-args args who default-vals known has-kw-rest)
    (if (null? args)
      default-vals
      (let* ([arg (car args)]
             [arg-nv (kw-arg-name.value arg)]) 
        (if (kw-arg? arg)
          (if (or has-kw-rest (member (car arg-nv) known))
            (process-args (cdr args) who (cons arg-nv default-vals) known has-kw-rest)
            (unknown who (car arg-nv)))
          (assertion-violation who "not a keyword argument" arg)))))
  
  (define (get-kw-val who arg-name default-vals)
    (if (null? default-vals)
      (missing who arg-name)
      (let ([kw.val (car default-vals)])
        (if (eq? arg-name (car kw.val))
          (cdr kw.val)
          (get-kw-val who arg-name (cdr default-vals))))))
  
  (define-syntax lambda/kw--meta
    (lambda (stx)
      (syntax-case stx ()      
        [(_ who eval-time-or-run-time (kw-arg* ... . kw-rest) body* ...)
         (let ([info 
                (map (lambda (ka)
                       (syntax-case ka ()
                         [(kw de) (cons #'kw #'de)]
                         [kw #'kw])) 
                     #'(kw-arg* ...))])
           (with-syntax ([(arg-name* ...) 
                          (map (lambda (i) (if (pair? i) (car i) i)) info)]
                         [defaults-expr 
                          (with-syntax ([((kw . de) ...) (filter pair? info)])
                            #'(list (cons 'kw de) ...))]
                         [ret-kw-alist 
                          (if (identifier? #'kw-rest)
                            #'kw-rest
                            (car (generate-temporaries #'(kw-rest))))]
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
                         (kw-lambda '())]))
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
                               (kw-lambda '())])])
                    kw-lambda)])))])))
  
  (define (check-kw-args incoming input-arg-names dflt-names has-kw-rest who)
    ;; NOTE: Called at expand-time.
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
    
  (define-syntax define/kw--meta
    (lambda (stx)
      (syntax-case stx ()
        [(_ name eval-time-or-run-time (kw-arg* ... . kw-rest) body* ...)
         (with-syntax ([(input-arg-name* ...) 
                        (kw-arg*->ids #'(kw-arg* ...))])
           (with-syntax ([(apply-arg-name* ...)
                          (generate-temporaries #'(input-arg-name* ...))])
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
                                [(run-time) #'(dflt-expr* ...)])]                             
                             [(dt* ...)  ;; used when kw-rest expression needs to capture defaults
                              (generate-temporaries #'(dflt-temp* ...))])
                 #'(begin
                     #;(define first-class                  
                     ;; the lambda/kw--meta will: 
                     ;;   at expand time check syntax of (kw-arg* ... . kw-rest) body* ...
                     ;;   and at run-time (when first-class called) process+check the args
                     #;(lambda/kw--meta name orig-stx eval-time-or-run-time (kw-arg* ... . kw-rest)
                     (the-proc arg-name* ... . ka)))
                     (define (the-proc input-arg-name* ... . kw-rest) 
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
                             [(s)
                              #'(s [:-])]
                             [(_ [:- [kw* val-expr*] (... ...)])         ;;;; call pattern
                              (and (for-all identifier? #'(kw* (... ...)))
                                   (check-kw-args (syntax->datum #'(kw* (... ...)))
                                                  '(input-arg-name* ...)
                                                  '(dflt-name* ...)
                                                  (identifier? #'kw-rest)
                                                  'name))
                              ;; Process keyword arguments at expand-time
                              (with-syntax ([(kwt* (... ...))
                                             (let f ([l (map (lambda (kw)
                                                               (cond [(assp (lambda (ian) 
                                                                              (free-identifier=? kw
                                                                                                 ian))
                                                                            (list
                                                                              (cons #'input-arg-name*
                                                                                    #'apply-arg-name*)
                                                                              ...))
                                                                      => cdr]
                                                                     [else  ;; kw is unknown
                                                                      (assert (identifier? #'kw-rest))
                                                                      ;; Give this unknown kw-arg a
                                                                      ;; new temp id so that it will
                                                                      ;; be put in kw-rest
                                                                      (car (generate-temporaries 
                                                                             (list kw)))]))
                                                             #'(kw* (... ...)))]
                                                     [a '()])
                                                (if (null? l) (reverse a)
                                                  ;; Give dup kw-args a new temp id, so that the last
                                                  ;; one is captured and passed to the-proc, and so all
                                                  ;; the dups can be put in kw-rest
                                                  (f (cdr l)
                                                     (cons (let ([x (car l)])
                                                             (if (memp (lambda (y)
                                                                         (free-identifier=? x y))
                                                                       (cdr l))
                                                               (car (generate-temporaries (list x)))
                                                               x)) 
                                                           a))))])
                                (with-syntax ([(kw*.kwt*-r (... ...))
                                               ;; so last dup is returned by assoc/assv/assq
                                               (reverse #'((cons 'kw* kwt*) (... ...)))])
                                  #'(let-syntax ([maybe-dt  
                                                  (lambda (stx)
                                                    (syntax-case stx ()
                                                      [(_ clause expr) 
                                                       (if (identifier? #'kw-rest)
                                                         #'(let clause expr)
                                                         #'expr)]))]
                                                 [call-tp
                                                  (lambda (stx)
                                                    (syntax-case stx (kwr)
                                                      [(_ p a* ((... ...) (... ...)) 
                                                          (kwr e* ((... ...) (... ...))))
                                                       (if (identifier? #'kw-rest)
                                                         #'(p a* ((... ...) (... ...)) 
                                                              e* ((... ...) (... ...)))
                                                         #'(p a* ((... ...) (... ...))))]))])     
                                      (let ([dflt-temp* dflt-val-expr*]
                                            ...)
                                        (maybe-dt ([dt* dflt-temp*] 
                                                   ...)
                                                  (let ([kwt* val-expr*]  ;; these shadow dflt-temp*
                                                        (... ...))
                                                    (call-tp the-proc apply-arg-name* ...
                                                             (kwr kw*.kwt*-r (... ...) 
                                                                  (cons 'dflt-name* dt*)
                                                                  ...))))))))]))))
                     . def-etime-dflts)))))])))
  
  (define-syntax lambda/kw
    (lambda (stx)
      (syntax-case stx ()
        [(_ kw-formals body0 body* ...)
         (check-kw-formals #'kw-formals stx)
         #'(lambda/kw--meta <a-lambda/kw> eval-time
             kw-formals body0 body* ...)])))
  
  (define-syntax lambda/kw/r
    (lambda (stx)
      (syntax-case stx ()
        [(_ kw-formals body0 body* ...)
         (check-kw-formals #'kw-formals stx)
         #'(lambda/kw--meta <a-lambda/kw/r> run-time
             kw-formals body0 body* ...)])))
  
  (define-syntax define/kw
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) body0 body* ...)
         (and (identifier? #'name)
              (check-kw-formals #'kw-formals stx))
         #'(define/kw--meta name eval-time kw-formals body0 body* ...)])))
  
  (define-syntax define/kw/r
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) body0 body* ...)
         (and (identifier? #'name)
              (check-kw-formals #'kw-formals stx))
         #'(define/kw--meta name run-time kw-formals body0 body* ...)])))
  
)
