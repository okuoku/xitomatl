#!r6rs
(library (xitomatl smatch (3))
  (export
    smatch
    smatch-lambda smatch-lambda*
    smatch-let smatch-let*)
  (import
    (rnrs)
    (xitomatl irregex)
    (only (xitomatl records) record-type-accessors)
    (for (only (xitomatl macro-utils) identifier?/name=? unique-ids?/raise) expand)
    (for (only (xitomatl indexes) enumerate) expand))
  
  (define-syntax smatch
    (lambda (in-stx)
      (define (P pat-stx)
        (syntax-case pat-stx ()
          ;; empty list
          [()
           #'(M-null)]
          ;; anything, ignore, don't bind
          [underscore
           (identifier?/name=? #'underscore '_)
           #'(M-ignore)]
          ;; anything, do bind
          [var
           (identifier? #'var)
           #'(M-variable
              var)]
          ;; quoted datum
          [(q datum)
           (identifier?/name=? #'q 'quote)
           #'((make-matcher M-datum (quote datum)))]
          ;; quasiquote datum 
          [(qq datum)
           (identifier?/name=? #'qq 'quasiquote)
           #'((make-matcher M-datum (quasiquote datum)))]
          ;; and
          [(and pat ...)
           (identifier?/name=? #'and 'and)
           (with-syntax ([((M V ...) ...) (map P #'(pat ...))])
             #'((make-matcher M-and M ...)
                V ... ...))]
          ;; or
          [(or pat ...)
           (identifier?/name=? #'or 'or)
           (with-syntax ([((M V ...) ...) (map P #'(pat ...))])
             (let ([Vs #'((V ...) ...)])
               (when (positive? (length Vs))
                 (unless (let ([syms (map syntax->datum (car Vs))])
                           (for-all (lambda (x)
                                      (equal? syms (map syntax->datum x)))
                                    (cdr Vs)))
                   (syntax-violation #f "or pattern variables mismatch" 
                                     in-stx pat-stx)))
               (with-syntax ([(V ...) (if (positive? (length Vs))
                                        (car Vs)
                                        '())])
                 #'((make-matcher M-or M ...)
                    V ...))))]
          ;; not
          [(not pat)
           (identifier?/name=? #'not 'not)
           (with-syntax ([(M V ...) (P #'pat)])
             (when (positive? (length #'(V ...)))
               (syntax-violation #f "not pattern contains variables" in-stx pat-stx))
             #'((make-matcher M-not M)))]
          [(not . _)
           (identifier?/name=? #'not 'not)
           (syntax-violation #f "invalid not pattern" in-stx pat-stx)]
          ;; string, according to IrRegex regular expression
          [(^$ irx pat ...)
           (identifier?/name=? #'^$ '^$)
           (with-syntax ([irx (if (string? (syntax->datum #'irx))
                                #'(irregex irx 'single-line 'fast)
                                #'irx)]
                         [(idx ...) (enumerate #'(pat ...))]
                         [((M V ...) ...) (map P #'(pat ...))])
             #'((make-matcher M-irregex irx '(idx ...) M ...)
                V ... ...))]
          [(^$ . _)
           (identifier?/name=? #'^$ '^$)
           (syntax-violation #f "invalid irregex pattern" in-stx pat-stx)]
          ;; record
          [(& rtype pat ...)
           (and (identifier?/name=? #'& '&)
                (or (identifier? #'rtype)
                    (syntax-case #'rtype () 
                      [(RTD x) (identifier?/name=? #'RTD 'RTD) #t]
                      [_ #f])))
           (with-syntax ([rtd-expr 
                          (syntax-case #'rtype () 
                            [(RTD x) (identifier?/name=? #'RTD 'RTD) #'x]
                            [rtype-stx #'(record-type-descriptor rtype-stx)])]
                         [num (length #'(pat ...))]
                         [((M V ...) ...)
                          (map P #'(pat ...))])
             #'((make-matcher M-record rtd-expr num M ...)
                V ... ...))]
          [(& . _)
           (identifier?/name=? #'& '&)
           (syntax-violation #f "invalid record pattern" in-stx pat-stx)]
          ;; arbitrary predicate
          [(? pred)
           (identifier?/name=? #'? '?)
           #'((make-matcher M-predicate pred))]
          [(? . _)
           (identifier?/name=? #'? '?)
           (syntax-violation #f "invalid predicate pattern" in-stx pat-stx)]
          ;; pair / list
          [(pat-car . pat-cdr)
           (with-syntax ([(car-M car-V ...) (P #'pat-car)]
                         [(cdr-M cdr-V ...) (P #'pat-cdr)])
             #'((make-matcher M-pair car-M cdr-M)
                car-V ... cdr-V ...))]
          ;; vector
          [#(pat ...)
           (with-syntax ([((M V ...) ...) (map P #'(pat ...))])
             (with-syntax ([len (length #'(pat ...))])
               #'((make-matcher M-vector len M ...)
                  V ... ...)))]
          ;; self-quoting datum
          [const
           #'((make-matcher M-datum const))])) 
      ;; start transforming
      (syntax-case in-stx () 
        [(_ expr clause0 clause ...)
         (with-syntax 
             ([((matcher 
                 fender-proc ...
                 true-expr-proc)
                ...)
               (map (lambda (c) 
                      (syntax-case c ()
                        [(pattern fender ... true-expr)
                         (<= 0 (length #'(fender ...)) 1)
                         (with-syntax 
                             ([(M V ...) (P #'pattern)])
                           (unique-ids?/raise #'(V ...) in-stx)
                           #'(M 
                              (lambda (V ...) fender) ...
                              (lambda (V ...) true-expr)))]
                        [_ (syntax-violation #f "invalid clause" in-stx c)]))
                    #'(clause0 clause ...))])
           ;; macro output
           #'(let ([obj expr])
               (cond
                 [(let ([vars (matcher obj '())])
                    (and vars
                         (let ([vars (reverse vars)])
                           (and (apply fender-proc vars) ...
                                vars))))
                  => (lambda (vars)
                       (apply true-expr-proc vars))]
                 ...
                 [else (assertion-violation 'smatch "failed to match" obj)])))])))
  
  ;;------------------------------------------------------------------------
  
  (define-syntax make-matcher
    (syntax-rules ()
      [(_ M args ...)
       (lambda (obj vars)
         (M obj vars args ...))]))

  (define (M-null obj vars)
    (and (null? obj)
         vars))
  
  (define (M-ignore obj vars)
    vars)
  
  (define (M-variable obj vars)
    (cons obj vars))
  
  (define (M-datum obj vars datum)
    (and (equal? datum obj)
         vars))
  
  (define (M-and obj vars . matchers)
    (let loop ([matchers matchers] 
               [vars vars])
      (if (pair? matchers)
        (let ([vars ((car matchers) obj vars)])
          (and vars 
               (loop (cdr matchers) vars)))
        vars)))
  
  (define (M-or obj vars . matchers)
    (let loop ([matchers matchers])
      (if (pair? matchers)
        (let ([vars ((car matchers) obj vars)])
          (or vars
              (loop (cdr matchers))))
        #f)))
  
  (define (M-not obj vars matcher)
    (if (matcher obj '())
      #f
      vars))
  
  (define (do-sub-matching sub-objs matchers vars)
    (let loop ([sub-objs sub-objs]
               [matchers matchers]
               [vars vars])
      (if (pair? matchers)
        (let ([vars ((car matchers) (car sub-objs) vars)])
          (and vars
               (loop (cdr sub-objs) 
                     (cdr matchers)
                     vars)))
        vars)))
  
  (define (M-irregex obj vars irx idxs . matchers)
    (and (string? obj)
         (let ([m (irregex-match irx obj)])
           (and m
                (= 0 (irregex-match-start m))
                (= (string-length obj) (irregex-match-end m))
                (do-sub-matching 
                 (map (lambda (i) (irregex-match-substring m i))
                      idxs)
                 matchers
                 vars)))))
  
  (define (M-record obj vars rtd num . matchers)
    (and ((record-predicate rtd) obj)
         (let ([fields-vals (map (lambda (a) (a obj))
                                 (record-type-accessors rtd))])
           (and (= num (length fields-vals))
                (do-sub-matching fields-vals matchers vars)))))
  
  (define (M-predicate obj vars pred)
    (and (pred obj)
         vars))
  
  (define (M-pair obj vars car-matcher cdr-matcher)
    (and (pair? obj)
         (let ([vars (car-matcher (car obj) vars)])
           (and vars
                (cdr-matcher (cdr obj) vars)))))
  
  (define (M-vector obj vars len . matchers)
    (and (vector? obj)
         (= len (vector-length obj))
         (do-sub-matching 
          (vector->list obj) 
          matchers
          vars)))
  
  ;;------------------------------------------------------------------------
  
  (define-syntax smatch-lambda
    (syntax-rules ()
      [(_ clause ...)
       (lambda (x) (smatch x clause ...))]))
  
  (define-syntax smatch-lambda*
    (syntax-rules ()
      [(_ clause ...)
       (lambda x (smatch x clause ...))]))
  
  (define-syntax smatch-let
    (syntax-rules ()
      [(_ ([pat expr] ...) body0 body ...) 
       (smatch (list expr ...) 
         [(pat ...) 
          (let () body0 body ...)])]))
  
  (define-syntax smatch-let*
    (syntax-rules ()
      [(_ () body0 body ...)
       (let () body0 body ...)]
      [(_ ([pat0 expr0] [pat expr] ...) body0 body ...)
       (smatch expr0 
         [pat0 
          (smatch-let* ([pat expr] ...) body0 body ...)])]))

)
