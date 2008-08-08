#!r6rs
(library (xitomatl keywords)
  (export
    keywords-parser
    case-lambda/kw lambda/kw define/kw
    keyword-condition? condition-keyword)
  (import
    (rnrs)
    (xitomatl match)
    (only (xitomatl conditions) make-predicate-condition)
    (for (only (xitomatl macro-utils) 
               with-syntax* gen-temp identifier?/name=?)
         expand))
  
  (define-syntax keywords-parser--meta
    (lambda (stx)
      (define (gen-kw-stx et-who rt-who process-input-list 
                          missing-keyword predicate-failed)
        (lambda (kw-spec kw-value)
          (define (invalid)
            (syntax-violation et-who "invalid options for keyword" stx kw-spec))
          (with-syntax ([(kw-id options ...) kw-spec])
            (let process-options ([options #'(options ...)]
                                  [default #f]
                                  [predicate #f]
                                  [boolean #f])
              (syntax-case options ()
                [(:default expr . rest)
                 (and (identifier?/name=? #':default ':default)
                      (not default))
                 (process-options #'rest #'expr predicate boolean)]
                [(:predicate expr . rest)
                 (and (identifier?/name=? #':predicate ':predicate)
                      (not predicate))
                 (process-options #'rest default #'expr boolean)]
                [(:boolean . rest)
                 (and (identifier?/name=? #':boolean ':boolean)
                      (not boolean))
                 (process-options #'rest default predicate #t)]
                [()
                 (cond [(and boolean (or default predicate))
                        (invalid)]
                       [(and default predicate)
                        (list #`[('kw-id v . r)
                                 (begin (set! #,kw-value v)
                                        (#,process-input-list r))]
                              #`(if (not-given? #,kw-value)
                                  #,default
                                  (if (#,predicate #,kw-value)
                                    #,kw-value
                                    (#,predicate-failed '#,rt-who 'kw-id
                                                        '#,predicate #,kw-value))))]
                       [default
                        (list #`[('kw-id v . r)
                                 (begin (set! #,kw-value v)
                                        (#,process-input-list r))]
                              #`(if (not-given? #,kw-value)
                                  #,default
                                  #,kw-value))]
                       [predicate
                        (list #`[('kw-id v . r)
                                 (begin (set! #,kw-value v)
                                        (#,process-input-list r))]
                              #`(if (not-given? #,kw-value)
                                  (#,missing-keyword '#,rt-who 'kw-id)
                                  (if (#,predicate #,kw-value)
                                    #,kw-value
                                    (#,predicate-failed '#,rt-who 'kw-id
                                                        '#,predicate #,kw-value))))]
                       [boolean
                        (list #`[('kw-id . r)
                                 (begin (set! #,kw-value #t)
                                        (#,process-input-list r))]
                              #`(not (not-given? #,kw-value)))]
                       [else
                        (list #`[('kw-id v . r)
                                 (begin (set! #,kw-value v)
                                        (#,process-input-list r))]
                              #`(if (not-given? #,kw-value)
                                  (#,missing-keyword '#,rt-who 'kw-id)
                                  #,kw-value))])]
                [_ (invalid)])))))
      (syntax-case stx ()
        [(_ et-who rt-who missing-value missing-keyword predicate-failed
            [kw-id options ...] ...)
         (for-all identifier? 
                  #'(missing-value missing-keyword predicate-failed kw-id ...))         
         (with-syntax* ([(kw-value ...) (generate-temporaries #'(kw-id ...))]
                        [process-input-list (gen-temp)]
                        [((match-clause value-expr) ...)
                         (map (gen-kw-stx 
                               (syntax->datum #'et-who) #'rt-who #'process-input-list
                               #'missing-keyword #'predicate-failed) 
                              #'([kw-id options ...] ...)
                              #'(kw-value ...))])
           #'(lambda (input-list)
               (let ([kw-value not-given] 
                     ...
                     [additional '()])               
                 (let process-input-list ([l input-list])
                   (match l
                     match-clause
                     ...
                     [() 
                      (set! additional (reverse additional))]
                     ['(kw-id)
                      (missing-value 'rt-who 'kw-id)]
                     ...
                     [(unknown . rest)
                      (begin (set! additional (cons unknown additional))
                             (process-input-list rest))]
                     [_ 
                      (assertion-violation 
                       'rt-who "not a proper list" input-list)]))                   
                 (letrec* ([kw-id value-expr] 
                           ...)
                   (values kw-id ... additional)))))])))
  
  (define-syntax keywords-parser/who
    (syntax-rules ()
      [(_ et-who rt-who . r)
       (keywords-parser--meta et-who rt-who
        missing-value--default missing-keyword--default predicate-failed--default
        . r)]))
  
  (define-syntax keywords-parser
    (syntax-rules ()
      [(_ . r)
       (keywords-parser/who keywords-parser "a keywords parser" . r)]))
  
  (define-condition-type &keyword &condition
    make-keyword-condition keyword-condition?
    (keyword condition-keyword))

  (define (AV who msg kw-id . more)
    (raise
     (apply condition
            (make-assertion-violation)
            (make-who-condition who)
            (make-message-condition msg)
            (make-keyword-condition kw-id)
            more)))
  
  (define (missing-value--default who kw-id)
    (AV who "keyword missing value" kw-id))

  (define (missing-keyword--default who kw-id)
    (AV who "missing required keyword" kw-id))

  (define (predicate-failed--default who kw-id pred-form value)
    (AV who "keyword predicate failed" kw-id 
        (make-predicate-condition pred-form)
        (make-irritants-condition (list value))))

  (define not-given
    (let ()
      (define-record-type not-given)
      (make-not-given)))

  (define (not-given? x) (eq? x not-given))

  ;;--------------------------------------------------------------------------
  
  (define-syntax case-lambda/kw--meta
    (lambda (stx)
      (define (parse-kw-formals et-who kw-formals)
        (let parse ([kwf kw-formals]
                    [pos-id '()])
          (syntax-case kwf ()
            [(pos . r) 
             (identifier? #'pos)
             (parse #'r (cons #'pos pos-id))]
            [([kw-id . opts] ... . additional-id)
             (and (for-all identifier? #'(kw-id ...))
                  (or (null? (syntax->datum #'additional-id))
                      (identifier? #'additional-id)))
             (list #'([kw-id . opts] ...)
                   (reverse pos-id)
                   (syntax-case #'additional-id ()
                     [() (gen-temp)]  ;; ignored
                     [_ #'additional-id]))]
            [_ (syntax-violation et-who "invalid keyword formals" stx kw-formals)])))
      (syntax-case stx ()
        [(_ et-who rt-who [kw-formals . body])
         (with-syntax ([(([kw-id . opts] ...) (pos-id ...) additional-id)
                        (parse-kw-formals (syntax->datum #'et-who) #'kw-formals)])
           #'(let ([parser 
                    (keywords-parser/who et-who rt-who [kw-id . opts] ...)])
               (lambda (pos-id ... . keywords)
                 (let-values ([(kw-id ... additional-id)
                               (parser keywords)])
                   . body))))]
        [(_ et-who rt-who [kw-formals . body] ...)
         (with-syntax ([(parser ...) (generate-temporaries #'(kw-formals ...))]
                       [((([kw-id . opts] ...) (pos-id ...) additional-id) ...)
                        (map (lambda (kwf) 
                               (parse-kw-formals (syntax->datum #'et-who) kwf))
                             #'(kw-formals ...))])
           #'(let ([parser 
                    (keywords-parser/clause-failed et-who [kw-id . opts] ...)]
                   ...)
               (let ([procs (list (cons
                                   (length '(pos-id ...))
                                   (lambda (pos-id ... . keywords)
                                     (let-values ([(kw-id ... additional-id)
                                                   (parser keywords)])
                                       . body)))
                                  ...)])
                 (lambda args
                   (let ([len (length args)])
                     (let try-next ([procs procs])
                       (if (pair? procs)
                         (if (>= len (caar procs))
                           ((call/cc
                             (lambda (k)
                               (with-exception-handler
                                 (lambda (ex)
                                   (if (clause-failed? ex)
                                     (k (lambda () (try-next (cdr procs))))
                                     (raise-continuable ex)))
                                 (lambda ()
                                   (let-values ([vals (apply (cdar procs) args)])
                                     (lambda () (apply values vals))))))))
                           (try-next (cdr procs)))
                         (apply assertion-violation 'rt-who
                                "no clause matches arguments" args))))))))])))
    
  (define-syntax keywords-parser/clause-failed
    (syntax-rules ()
      [(_ et-who . r)
       (keywords-parser--meta et-who ignored
        missing-value/clause-failed 
        missing-keyword/clause-failed
        predicate-failed/clause-failed
        . r)]))

  (define clause-failed
    (let ()
      (define-record-type clause-failed)
      (make-clause-failed)))

  (define (clause-failed? x) (eq? x clause-failed))

  (define (missing-value/clause-failed who kw-id)
    (raise clause-failed))

  (define (missing-keyword/clause-failed who kw-id)
    (raise clause-failed))

  (define (predicate-failed/clause-failed who kw-id pred-form value)
    (raise clause-failed))
  
  (define-syntax case-lambda/kw
    (syntax-rules ()
      [(_ [kw-formals body0 body ...] ...)
       (case-lambda/kw--meta case-lambda/kw "a case-lambda/kw procedure"
                             [kw-formals body0 body ...] ...)]))
  
  (define-syntax lambda/kw
    (syntax-rules ()
      [(_ kw-formals body0 body ...)
       (case-lambda/kw--meta lambda/kw "a lambda/kw procedure"
                             [kw-formals body0 body ...])]))

  (define-syntax define/kw
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) body0 body ...)
         (identifier? #'name)
         #'(define name
             (case-lambda/kw--meta define/kw name
                                   [kw-formals body0 body ...]))])))
  
  ;;--------------------------------------------------------------------------
  ;; Below is a half-completed define/kw which does an optimization of
  ;; processing the input keywords and arguments at expand time.  But I
  ;; started thinking the implementation complexity it requires is not
  ;; worth the maintenance burden.
#|
  (define-syntax define/kw
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . kw-formals) . body)
         (with-syntax* ([first-class (identifier-append #'here #'name '--first-class)]
                        [(([kw-id . opts] ...) (pos-id ...) additional-id)
                         (parse-kw-formals 'define/kw #'kw-formals)]
                        [(kw-value ...) (generate-temporaries #'(kw-id ...))]
                        [process-input-list (gen-temp)]
                        [((match-clause value-expr) ...)
                         (map (gen-kw-stx 
                               'define/kw (syntax->datum #'name) process-input-list
                               #'missing-keyword--default #'predicate-failed--default) 
                              #'([kw-id . opts] ...)
                              #'(kw-value ...))])
           #'(begin
               (define (proc pos-id ... kw-value ... additional-id)
                 (letrec* ([kw-id value-expr]
                           ...)
                   . body))
               (define first-class
                 (lambda/kw kw-formals
                   (proc pos-id ... kw-id ... additional-id)))
               (define-syntax name
                 (lambda (stx)
                   (define (process-args args-stx)
                     (let ([kw-value #'not-given] 
                           ...
                           [additional '()])               
                       (let process-input-list ([l args-stx])
                         (match (if (pair? args-stx)
                                  (cons (syntax->datum (car args-stx))
                                        (cdr args-stx))
                                  args-stx)
                           match-clause
                           ...
                           [() 
                            (set! additional (reverse additional))]
                           ['(kw-id)
                            ???(missing-value 'rt-who 'kw-id)]
                           ...
                           [(unknown . rest)
                            (begin (set! additional (cons (car args-stx) additional))
                                   (process-input-list rest))]
                           #;[_ 
                            (assertion-violation 
                             'define/kw "not a proper list" input-list)]))
                       ???(values kw-id ... additional)))
                   (syntax-case stx ()
                     [(_ args (... ...))
                      (with-syntax ([(pos-expr (... ...) kw-expr (... ...) additional-expr)
                                     (process-args #'(args (... ...)))])
                        #'(proc pos-expr (... ...) kw-expr (... ...) additional-expr))]
                     [id
                      (identifier? #'id)
                      #'first-class])))))])))
|#
)
