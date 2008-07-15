#!r6rs
(library (xitomatl smatch (0 2))
  (export
    smatch
    smatch-lambda smatch-lambda*
    smatch-let smatch-let*)
  (import
    (rnrs)
    (for (only (xitomatl indexes) enumerate) expand)
    (for (only (xitomatl macro-utils) with-syntax* unique-ids?/raise gen-temp) expand)
    (xitomatl irregex)
    (only (xitomatl records) record-type-accessors))
  
  (define-syntax smatch
    (lambda (in-stx)
      (define (underscore? x)
        (and (identifier? x) (free-identifier=? x #'_)))
      (define (fresh-T)
        (define accum-vars '())
        (define (accum-var! var-stx)
          (let ([n (cons var-stx accum-vars)])
            (when (unique-ids?/raise n in-stx)
              (set! accum-vars n))))
        (define (T stx)
          (syntax-case stx () [(obj . r) (assert (identifier? #'obj))])
          (syntax-case stx (quote quasiquote ^$ & and or not)
            ;; no fender, add "always true" one
            [(obj pattern body)
             (T #'(obj pattern #t body))]
            ;; empty list
            [(obj () fender body)
             #'(and (null? obj) fender body)]
            ;; anything, ignore, don't bind
            [(obj underscore fender body)
             (underscore? #'underscore)
             #'(and fender body)]
            ;; anything, do bind
            [(obj var fender body)
             (identifier? #'var)
             (begin (accum-var! #'var)
                    #'(let ([var obj]) (and fender body)))]
            ;; quoted datum
            [(obj (quote datum) fender body)
             #'(and (equal? obj (quote datum)) fender body)]
            ;; quasiquote datum 
            [(obj (quasiquote datum) fender body)
             #'(and (equal? obj (quasiquote datum)) fender body)]
            ;; string, according to IrRegex regular expression
            [(obj (^$ irx pat ...) fender body)
             (with-syntax ([irx (syntax-case #'irx (quote)
                                  [str (string? (syntax->datum #'str))
                                   #`(irregex #,(string-append (syntax->datum #'str) "$")
                                              'single-line 'fast)]
                                  [(quote sre) #'(quote (seq sre eos))]
                                  [_ #'irx])]
                           [(idx ...) (enumerate #'(pat ...))])
               #`(and (string? obj)
                      (let ([m (irregex-match irx obj)])
                        (and m
                             (let ([sub-strs (list (irregex-match-substring m idx) ...)])
                               #,(T #'(sub-strs (pat ...) fender body)))))))]
            [(obj (^$ . r) fender body)
             (syntax-violation #f "invalid irregex pattern" in-stx #'(^$ . r))]
            ;; record
            [(obj (& rtype pat ...) fender body)
             (or (identifier? #'rtype)
                 (syntax-case #'rtype (RTD) [(RTD x) #t] [_ #f]))
             (with-syntax* ([rtd-expr
                             (syntax-case #'rtype (RTD)
                               [(RTD x) #'x]
                               [rtype-stx #'(record-type-descriptor rtype)])]
                            [destructure-expr
                             #`(let* ([rtd rtd-expr]
                                      [pred (record-predicate rtd)])
                                 (and (pred obj)
                                      (let ([field-vals (map (lambda (a) (a obj))
                                                             (record-type-accessors rtd))])
                                        #,(T #'(field-vals (pat ...) fender body)))))])
               (if (identifier? #'rtype)
                 #'(and (record? obj)  ;; this prevents opaque records from matching
                        destructure-expr)
                 #'destructure-expr))]
            [(_ (& . _) _ _)
             (syntax-violation #f "invalid record pattern" in-stx #'(& . r))]
            ;; and
            [(obj (and pat0 pat ...) fender body)
             (T #`(obj pat0 #,(T #'(obj (and pat ...) fender body))))]
            [(obj (and) fender body)
             #'(and fender body)]
            ;; or -- probably not good because it allows an unpredictable set of variables
            #|[(obj (or pat0 pat ...) fender body)
             #`(or #,(T #'(obj pat0 fender body))
                   #,(T #'(obj (or pat ...) fender body)))]
            [(obj (or) fender body)
             #f]|#
            ;; not
            [(obj (not pat) fender body)
             #`(if #,(T #'(obj pat #t))
                 #f
                 (and fender body))]
            [(_ (not . r) _ _)
             (syntax-violation #f "invalid not pattern" in-stx #'(not . r))]
            ;; pair / list
            [(obj (pat-car . pat-cdr) fender body)
             (with-syntax ([(obj-car obj-cdr) (generate-temporaries '(1 2))])
               #`(and (pair? obj)
                      (let ([obj-car (car obj)] [obj-cdr (cdr obj)])
                        #,(T #`(obj-car pat-car 
                                        #,(T #'(obj-cdr pat-cdr fender body)))))))]
            ;; vector
            [(obj #(pat* ...) fender body)
             #`(and (vector? obj)
                    (= (vector-length obj) #,(length #'(pat* ...)))
                    #,(if (for-all identifier? #'(pat* ...)) ;; optimize for this case
                        (with-syntax ([(idx* ...) (enumerate #'(pat* ...))])
                          (for-each accum-var! #'(pat* ...))
                          #'(let ([pat* (vector-ref obj idx*)] ...)
                              (and fender body)))
                        #`(let ([l (vector->list obj)])
                            #,(T #'(l (pat* ...) fender body)))))]
            ;; self-quoting datum
            [(obj const fender body)
             #'(and (equal? obj const) fender body)]))
        T) 
      ;; start transforming
      (syntax-case in-stx () 
        [(_ expr clause ...)
         (with-syntax* ([obj (gen-temp)]
                        [(test ...)
                         (map (lambda (c) 
                                (syntax-case c ()
                                  [(pattern fender ... body)
                                   (<= 0 (length #'(fender ...)) 1)
                                   ((fresh-T) 
                                    #'(obj pattern fender ... 
                                           (let-values ([vs body]) vs)))]
                                  [_ (syntax-violation #f "invalid clause" in-stx c)]))
                              #'(clause ...))])
           #'(let ([obj expr])
               (cond 
                 [test => (lambda (vs) (apply values vs))]
                 ...
                 [else (assertion-violation 'smatch "failed to match" obj)])))])))
  
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
      [(_ ([pat* expr*] ...) body0 body* ...) 
       (smatch (list expr* ...) 
         [(pat* ...) 
          (let () body0 body* ...)])]))
  
  (define-syntax smatch-let*
    (syntax-rules ()
      [(_ () body0 body* ...)
       (let () body0 body* ...)]
      [(_ ([pat expr] [pat* expr*] ...) body0 body* ...)
       (smatch expr 
         [pat 
          (smatch-let* ([pat* expr*] ...) body0 body* ...)])]))

)
