;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pattern matcher by Derick Eddington (derick.eddington@gmail.com)
;;; 
;;; Somewhat similar to the common Wright matcher, but different.
;;; Distinguishing features:
;;; - Regular expression matching against strings, with sub-group matching. 
;;; - Record matching, with field matching. 
;;; - Arbitrary predicate matching.
;;; - "and", "or", and "not" matching.
;;; - quasiquote patterns, with the unquote'd expressions evaluated as 
;;;   normal expressions in the environment of the match expression.
;;; - "..." multiple elements matching, with specifiable minimum and maximum.
;;; - Multiple "..." patterns in the same pattern.
;;; - "(x ... . r)" pattern matches a possibly empty chain of pairs,
;;;   like syntax-case.
;;; - "..." pattern works with every compound pattern type.
;;; - Abstracted design.  syntax-case procedural macros ease implementation.
;;;   The executed expanded form uses procedural abstraction instead of
;;;   generating redundant code.
;;; - Efficient execution.
;;; - Functional, i.e., no mutation.
;;;
;;; Grammar:
;;; 
;;; (match <expr> <clause> <clause> ...)
;;; (match-let ([<pat> <expr>] ...) <body>)
;;; (match-let* ([<pat> <expr>] ...) <body>)
;;; (match-lambda <clause> <clause> ...)
;;; (match-lambda* <clause> <clause> ...)
;;; 
;;; <clause> ::= (<pat> <expr>)
;;;            | (<pat> <fender> <expr>)
;;; <fender> ::= <expr>
;;; <pat>                            Matches: 
;;;  ::= _                             Anything, does not bind
;;;    | <pat-var>                     Anything, bind variable
;;;    | <constant>                    Datum, according to equal?
;;;    | (quote <datum>)               Datum, according to equal?
;;;    | (quasiquote <qq-template>)    Datum, according to equal?
;;;    | ()                            Empty list
;;;    | (<pat> . <pat>)               Pair
;;;    | (<pat> <ooo> . <pat>)         Chain of pairs, possibly empty
;;;    | #(<vec-pat> ...)              Vector
;;;    | (:and <pat> ...)              If all sub-patterns match value
;;;    | (:or <pat> ...)               If any sub-pattern matches value
;;;    | (:not <pat>)                  If sub-pattern does not match value
;;;    | (:regex <irx> <pat> ...)      String, if it matches the regular
;;;                                    expression and if the captured groups
;;;                                    (which are strings) match sub-patterns
;;;    | (:record <r-type> <pat> ...)  Record of specified type,
;;;                                    whose fields' values match sub-patterns
;;;    | (:predicate <expr>)           If result of expression applied to value
;;;                                    returns true
;;; <pat-var>  ::= Any <identifier> except: 
;;;                ... quote quasiquote :and :or :not :regex :record :predicate
;;; <constant> ::= <boolean> | <number> | <character> 
;;;              | <string> | <bytevector>
;;; <ooo>      ::= ... | (... <integer>) | (... <integer> <integer>)
;;; <vec-pat>  ::= <pat> | <pat> <ooo>
;;; <irx>      ::= <expr> which evaluates to a valid irregex for Alex Shinn's
;;;                IrRegular Expressions library.
;;;                I.e., a string, SRE, or compiled irregex.
;;; <r-type>   ::= R6RS <record-name> handle for the record type.  This gets
;;;                wrapped with record-type-descriptor.
;;;              | (RTD <expr>) where the expression evaluates to a first-class
;;;                record type descriptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!r6rs
(library (xitomatl match (1))
  (export
    match matches?
    match-lambda match-lambda*
    match-let match-let*)
  (import
    (rnrs)
    (only (xitomatl irregex (or (0 6 (>= 2)) (0 (>= 7)) ((>= 1))))
          irregex irregex-match irregex-match-substring)
    (only (xitomatl records)
          record-type-accessors)
    (only (xitomatl vectors)
          subvector)
    (for (only (xitomatl macro-utils)
               identifier?/name=? name=? unique-ids?/raise)
         expand)
    (for (only (xitomatl indexes)
               enumerate)
         expand)
    (for (only (xitomatl predicates)
               exact-non-negative-integer? exact-positive-integer?)
         expand))
  
  (define-syntax match
    (lambda (in-stx)
      (define (keyword? pat-stx)
        (and (identifier? pat-stx)
             (exists (lambda (x) (name=? pat-stx x)) 
                     '(quote quasiquote :and :or :not :regex :record :predicate ...))))
      (define (ooo-range-valid? ooo-stx)
        (syntax-case ooo-stx ()
          [(min max)
           (let ([min (syntax->datum #'min)]
                 [max (syntax->datum #'max)])
             (and (exact-non-negative-integer? min)
                  (or (not max)
                      (and (exact-positive-integer? max)
                           (<= min max)))))]))
      (define (ooo? ooo-stx)
        (syntax-case ooo-stx ()
          [(ooo min)
           (and (identifier?/name=? #'ooo '...)
                (ooo-range-valid? #'(min #f)))]
          [(ooo min max)
           (and (identifier?/name=? #'ooo '...)
                (ooo-range-valid? #'(min max)))]
          [ooo
           (identifier?/name=? #'ooo '...)]
          [_ #f]))
      (define (ooo-range ooo-stx)
        (syntax-case ooo-stx ()
          [(_ min)     #'(min #f)]
          [(_ min max) #'(min max)]
          [_           #'(0 #f)]))
      ;;; P does the core of the syntax logic.  It is given a syntax object of
      ;;; a match pattern.  It returns a syntax object that is a list whose
      ;;; first element is an expression which evaluates to a matcher procedure
      ;;; and whose, possibly empty, remaining elements are identifiers of
      ;;; pattern variables, in lexical left-to-right order, which are to be
      ;;; bound by the pattern.  P is used recursively as a recursive match
      ;;; pattern is parsed.
      (define (P pat-stx)
        (syntax-case pat-stx ()
          ;; empty list
          [()
           #'(M-null)]
          ;; anything, ignore, don't bind
          [underscore
           (identifier?/name=? #'underscore '_)
           #'(M-ignore)]
          ;; prevent misuse of pattern syntax keywords
          [invalid
           (keyword? #'invalid)
           (syntax-violation #f "misuse of pattern syntax" in-stx pat-stx)]          
          ;; anything, do bind
          [var
           (identifier? #'var)
           #'(M-variable
              var)]
          ;; quote'd datum
          [(q datum)
           (identifier?/name=? #'q 'quote)
           #'((make-matcher M-datum (quote datum)))]
          ;; quasiquote'd datum
          [(qq datum)
           (identifier?/name=? #'qq 'quasiquote)
           #'((make-matcher M-datum (quasiquote datum)))]
          ;; and
          [(:and pat ...)
           (identifier?/name=? #':and ':and)
           (with-syntax ([((M V ...) ...) (map P #'(pat ...))])
             #'((make-matcher M-and (vector M ...))
                V ... ...))]
          ;; or
          [(:or pat ...)
           (identifier?/name=? #':or ':or)
           (with-syntax ([((M V ...) ...) (map P #'(pat ...))])
             (let ([Vs #'((V ...) ...)])
               (when (positive? (length Vs))
                 (unless (let ([syms (map syntax->datum (car Vs))])
                           (for-all (lambda (x)
                                      (equal? syms (map syntax->datum x)))
                                    (cdr Vs)))
                   (syntax-violation #f ":or pattern variables mismatch" in-stx pat-stx)))
               (with-syntax ([(V ...) (if (positive? (length Vs))
                                        (car Vs)
                                        '())])
                 #'((make-matcher M-or (vector M ...))
                    V ...))))]
          ;; not
          [(:not pat)
           (identifier?/name=? #':not ':not)
           (with-syntax ([(M V ...) (P #'pat)])
             (when (positive? (length #'(V ...)))
               (syntax-violation #f ":not pattern contains variables" in-stx pat-stx))
             #'((make-matcher M-not M)))]
          ;; string, according to IrRegex regular expression
          [(:regex irx pat ...)
           (identifier?/name=? #':regex ':regex)
           (with-syntax ([irx (if (string? (syntax->datum #'irx))
                                #'(irregex irx 'single-line 'fast)
                                #'irx)]
                         [(idx ...) (map (lambda (i) (+ 1 i)) (enumerate #'(pat ...)))]
                         [((M V ...) ...) (map P #'(pat ...))])
             #'((make-matcher M-irregex irx '#(idx ...) (vector M ...))
                V ... ...))]
          ;; record
          [(:record rtype pat ...)
           (and (identifier?/name=? #':record ':record)
                (or (identifier? #'rtype)
                    (syntax-case #'rtype () 
                      [(RTD _) (identifier?/name=? #'RTD 'RTD) #t]
                      [_ #f])))
           (with-syntax ([rtd-expr 
                          (syntax-case #'rtype () 
                            [(_ x) #'x]
                            [x #'(record-type-descriptor x)])]
                         [num (length #'(pat ...))]
                         [((M V ...) ...) (map P #'(pat ...))])
             #'((make-matcher M-record rtd-expr num (vector M ...))
                V ... ...))]
          ;; arbitrary predicate
          [(:predicate pred)
           (identifier?/name=? #':predicate ':predicate)
           #'((make-matcher M-predicate pred))]
          ;; multiple elements of, possibly empty, chain of pairs
          [(pat ooo . pat-rest)
           (ooo? #'ooo)
           (with-syntax 
               ([(ooo-M ooo-V ...) (P #'pat)]
                [(min max) (ooo-range #'ooo)]
                [(rest-M rest-V ...) (P #'pat-rest)])
             #`((make-matcher M-pair-chain 
                              ooo-M min max 
                              (quote #,(map (lambda (_) '()) #'(ooo-V ...)))
                              rest-M)
                ooo-V ... rest-V ...))]
          ;; prevent misuse of pattern syntax keywords
          [(invalid . _)
           (keyword? #'invalid)
           (syntax-violation #f "misuse of pattern syntax" in-stx pat-stx)]          
          ;; pair / list / improper list
          [(pat-car . pat-cdr)
           (with-syntax ([(car-M car-V ...) (P #'pat-car)]
                         [(cdr-M cdr-V ...) (P #'pat-cdr)])
             #'((make-matcher M-pair car-M cdr-M)
                car-V ... cdr-V ...))]
          ;; multiple elements of vector
          [#(pat ...)
           (let scan ([pats #'(pat ...)] [preceded #f])
             (and (pair? pats)
                  (if (ooo? (car pats))
                    preceded
                    (scan (cdr pats) #t))))
           (let-values 
               ([(pats-preceding pat-ooo min max pats-rest)
                 (let scan ([pats #'(pat ...)] 
                            [preceding '()])
                   (let ([x (car pats)])
                     (if (ooo? x)
                       (with-syntax ([(min max) (ooo-range x)])
                         (values (reverse (cdr preceding))
                                 (car preceding)
                                 #'min #'max
                                 (cdr pats)))
                       (scan (cdr pats)
                             (cons x preceding)))))])
             (with-syntax 
                 ([((preceding-M preceding-V ...) ...) (map P pats-preceding)]
                  [(ooo-M ooo-V ...) (P pat-ooo)]
                  [(min max) (list min max)]
                  ;; NOTE: the rest is matched as a list
                  [(rest-M rest-V ...) (P pats-rest)])
               #`((make-matcher M-vector-ooo
                                (vector preceding-M ...)
                                ooo-M min max 
                                (quote #,(map (lambda (_) '()) #'(ooo-V ...)))
                                rest-M)
                  preceding-V ... ... ooo-V ... rest-V ...)))]
          ;; vector
          [#(pat ...)
           (with-syntax ([((M V ...) ...) (map P #'(pat ...))]
                         [len (length #'(pat ...))])
             #'((make-matcher M-vector len (vector M ...))
                V ... ...))]
          ;; self-quoting datum
          [const
           #'((make-matcher M-datum const))])) 
      ;; start transforming
      (syntax-case in-stx () 
        [(_ expr clause0 clause ...)
         (with-syntax 
             ([((matcher fender-proc ... true-expr-proc) ...)
               (map (lambda (c) 
                      (syntax-case c ()
                        [(pattern fender ... true-expr)
                         (<= (length #'(fender ...)) 1)
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
                 [(do-matching matcher obj fender-proc ...)
                  => (lambda (vars)
                       (apply true-expr-proc vars))]
                 ...
                 [else (failed-to-match obj)])))])))
  
  (define-syntax do-matching
    (syntax-rules ()
      [(_ matcher obj)
       (do-matching/no-fender matcher obj)]
      [(_ matcher obj fender)
       (do-matching/fender matcher obj fender)]))
  
  (define (do-matching/no-fender matcher obj)
    (let ([vars (matcher obj '())])
      (and vars
           (reverse vars))))
  
  (define (do-matching/fender matcher obj fender)
    (let ([vars (matcher obj '())])
      (and vars
           (let ([vars (reverse vars)])
             (and (apply fender vars)
                  vars)))))
  
  (define (failed-to-match obj)
    (assertion-violation 'match "failed to match" obj))
  
  ;;------------------------------------------------------------------------
  
  (define-syntax make-matcher
    (syntax-rules ()
      [(_ M args ...)
       (lambda (obj vars)
         (M obj vars args ...))]))
  
  ;;; `vars' in the below matchers is a list of the pattern variables' values,
  ;;; in the reverse order the values are extracted when destructuring, i.e.,
  ;;; accumulated in the order the values are extracted by cons'ing onto the
  ;;; head of the list.  This is also the reverse order of the variables'
  ;;; identifiers lexical occurance in the entire compound pattern.

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
  
  (define (M-and obj vars matchers)
    (let ([end (vector-length matchers)])
      (let loop ([i 0] [vars vars])
        (if (= i end)
          vars
          (let ([vars ((vector-ref matchers i) obj vars)])
            (and vars 
                 (loop (+ 1 i) vars)))))))
  
  (define (M-or obj vars matchers)
    (let ([end (vector-length matchers)])
      (let loop ([i 0])
        (if (= i end)
          #f
          (let ([vars ((vector-ref matchers i) obj vars)])
            (or vars
                (loop (+ 1 i))))))))
  
  (define (M-not obj vars matcher)
    (if (matcher obj '())
      #f
      vars))
  
  (define (do-sub-matching sub-objs matchers vars)    
    (let ([end (vector-length matchers)])
      (let loop ([i 0] [vars vars])
        (if (= i end)
          vars
          (let ([vars ((vector-ref matchers i) (vector-ref sub-objs i) vars)])
            (and vars
                 (loop (+ 1 i) vars)))))))
  
  ;; TODO?: Cache pre-compiled irregex'es for string irx below
  
  (define (M-irregex obj vars irx idxs matchers)
    (and (string? obj)
         (let ([m (irregex-match irx obj)])
           (and m
                (do-sub-matching 
                 (vector-map (lambda (i) (irregex-match-substring m i))
                             idxs)
                 matchers
                 vars)))))
  
  ;;; FIXME: need to use a weak hashtable
  #;(define rtd-ht (make-eq-hashtable))
  
  (define (M-record obj vars rtd num matchers)
    (and ((record-predicate rtd) obj)
         (let ([fields-vals (vector-map (lambda (a) (a obj))
                                        (record-type-accessors rtd))])
           (and (= num (vector-length fields-vals))
                (do-sub-matching fields-vals matchers vars))))
    #;(let ([predicate.accessors 
           (or (hashtable-ref rtd-ht rtd #f)
               (let ([predicate.accessors (cons (record-predicate rtd)
                                                (record-type-accessors rtd))])
                 (hashtable-set! rtd-ht rtd predicate.accessors)
                 predicate.accessors))])
      (and ((car predicate.accessors) obj)
           (let ([fields-vals (vector-map (lambda (a) (a obj))
                                          (cdr predicate.accessors))])
             (and (= num (vector-length fields-vals))
                  (do-sub-matching fields-vals matchers vars))))))
  
  (define (M-predicate obj vars pred)
    (and (pred obj)
         vars))
  
  (define (M-pair obj vars car-matcher cdr-matcher)
    (and (pair? obj)
         (let ([vars (car-matcher (car obj) vars)])
           (and vars
                (cdr-matcher (cdr obj) vars)))))
  
  (define (M-vector obj vars len matchers)
    (and (vector? obj)
         (= len (vector-length obj))
         (do-sub-matching obj matchers vars)))
  
  (define (M-pair-chain obj vars 
                        ooo-matcher min max 
                        empty-ooo-vars
                        rest-matcher)
    ;; In order to match rest-matcher against the end of the chain, we must
    ;; work backwards across the chain of pairs (otherwise it might match
    ;; before the end).  So we create a list of the pairs in reverse order
    ;; and use that.  This is more effecient than the non-tail-recursive
    ;; solution of a function which immediately recurs forwards across the
    ;; chains and does the backwards work as the recursive calls return.  The
    ;; stack space used by that solution significantly exceeds that of the
    ;; space used by the reversed chain list, and that solution does not 
    ;; return as a tail-return because it must test each recursive call's 
    ;; return value, which costs significantly more time.
    (let match-last ([rev (let reverse-chain ([obj obj] [rev '()])
                            (if (pair? obj)
                              (reverse-chain (cdr obj) (cons obj rev))
                              (cons obj rev)))])
      (and (pair? rev)
           (let ([rest-vars (rest-matcher (car rev) '())])
             (if rest-vars
               (let match-ooo ([rev (cdr rev)]
                               [accum-ooo-vars empty-ooo-vars]
                               [count 0])
                 (if (pair? rev)
                   (and (or (not max) 
                            (< count max))
                        (let ([ooo-vars (ooo-matcher (caar rev) '())])
                          (and ooo-vars
                               (match-ooo (cdr rev)
                                          (map cons ooo-vars accum-ooo-vars)
                                          (+ 1 count)))))
                   (and (>= count min)
                        (append rest-vars
                                accum-ooo-vars
                                vars))))
               (match-last (cdr rev)))))))
  
  (define (M-vector-ooo obj vars 
                        preceding-matchers
                        ooo-matcher min max
                        empty-ooo-vars
                        rest-matcher)
    (and (vector? obj)
         (let ([p-len (vector-length preceding-matchers)]
               [obj-len (vector-length obj)])
           (and (>= obj-len p-len)
                (let ([vars (do-sub-matching 
                             (subvector obj 0 p-len)
                             preceding-matchers
                             vars)])
                  (let match-last ([last '()] 
                                   [y (- obj-len 1)])
                    (let ([rest-vars (rest-matcher last '())])
                      (if rest-vars
                        (let match-ooo ([x y]
                                        [accum-ooo-vars empty-ooo-vars]
                                        [count 0])
                          (if (>= x p-len)
                            (and (or (not max) 
                                     (< count max))
                                 (let ([ooo-vars (ooo-matcher (vector-ref obj x) '())])
                                   (and ooo-vars
                                        (match-ooo (- x 1)
                                                   (map cons ooo-vars accum-ooo-vars)
                                                   (+ 1 count)))))
                            (and (>= count min)
                                 (append rest-vars
                                         accum-ooo-vars
                                         vars))))
                        (and (>= y p-len)
                             (match-last (cons (vector-ref obj y) last)
                                         (- y 1)))))))))))
  
  ;;------------------------------------------------------------------------
  
  (define-syntax matches?
    (syntax-rules ()
      [(_ pattern)
       (match-lambda
         [pattern #t]
         [_ #f])]))
  
  (define-syntax match-lambda
    (syntax-rules ()
      [(_ clause ...)
       (lambda (x) (match x clause ...))]))
  
  (define-syntax match-lambda*
    (syntax-rules ()
      [(_ clause ...)
       (lambda x (match x clause ...))]))
  
  (define-syntax match-let
    (syntax-rules ()
      [(_ ([pat expr] ...) body0 body ...) 
       (match (vector expr ...) 
         [#(pat ...) 
          (let () body0 body ...)])]))
  
  (define-syntax match-let*
    (syntax-rules ()
      [(_ () body0 body ...)
       (let () body0 body ...)]
      [(_ ([pat0 expr0] [pat expr] ...) body0 body ...)
       (match expr0 
         [pat0 
          (match-let* ([pat expr] ...) body0 body ...)])]))

)
