#!r6rs
(library (xitomatl generics)
  (export
    make-generic define-generic specialize)
  (import
    (rnrs))

  ;;; Generics are procedures which delegate to some underlying procedure 
  ;;; determined by the arguments to the generic.  A per-generic association
  ;;; of predicates to underlying-procedures is used to determine and find 
  ;;; what underlying to use.  No O.O.P. is involved, though it could be done
  ;;; on top.  Variable numbers of arguments ("rest arguments" lists) are fully
  ;;; supported.  Specializations are added at run-time, so new ones can be
  ;;; added by parties unknown to the creator of a generic.  Precedence is
  ;;; according to the temporal order specializations are added, which
  ;;; ensures the specializations added by the creator always have
  ;;; precedence.  Performance will lessen the more arguments a
  ;;; specialization has and will depend on how many other specializations
  ;;; precede it.  Being fully run-time dynamic with a single simple way of
  ;;; reconfiguring the specializations of a generic, the design is open to
  ;;; possible future abilities such as removing specializations or reordering
  ;;; their precedence.
  
  ;;; This special distinct object, only available inside this library,
  ;;; is used as a key to (re)configure the specializations of a generic.
  (define-record-type configure)
  (define C (make-configure))
  
  (define (specialize generic preds proc)
    ;;; `generic' is the generic to, dynamically at run-time, add a new
    ;;; specialization for.
    ;;; `preds' must be of the type <argument-predicates> described in the
    ;;; below comment in `make-generic' about `specializations'.  That is,
    ;;; `preds' must be: a possibly empty list of one-argument predicates which
    ;;; return true or #f, or an improper list of predicates of the type just
    ;;; described but with the final cdr being an any-number-of-arguments
    ;;; predicate which returns true or #f, or a predicate of the type just
    ;;; described for the final cdr of the improper list case.  This matches
    ;;; the <formals> specification of a procedure's arguments:
    ;;; (args ...) or (arg args ... . rest) or rest.
    ;;; `proc' is the underlying procedure delegated to for the arguments case
    ;;; specified by `preds'.  The number of arguments `proc' accepts must
    ;;; match those specified by `preds'.  That is, `proc' must accept as many
    ;;; arguments as there are one-argument predicates in `preds', and if there
    ;;; is an any-number-of-arguments "rest arguments" predicate in/as `preds',
    ;;; then `proc' must accept the additional, possibly variable, number of
    ;;; "rest arguments" that the any-number-of-arguments predicate returned
    ;;; true for.
    (define who 'specialize)
    (unless (procedure? generic)
      (assertion-violation who "not a generic" generic))
    (unless (valid-predicates-spec? preds)
      (assertion-violation who "invalid predicates specification" preds))
    (unless (procedure? proc)
      (assertion-violation who "not a procedure" proc))
    ;; append is used so that specializations have precedence according
    ;; to the order they were added.
    (generic C (lambda (specializations) 
                 (append specializations (list (cons preds proc))))))
  
  (define (valid-predicates-spec? x)
    (cond [(pair? x) (and (procedure? (car x)) (valid-predicates-spec? (cdr x)))]
          [(null? x) #t]
          [(procedure? x) #t]
          [else #f]))
  
  (define configure-preds (list configure? procedure?))
  
  (define make-generic
    (case-lambda
      [() (make-generic 'generic)]
      [(who)
       (unless (symbol? who)
         (assertion-violation 'make-generic "not a symbol" who))
       ;; specializations ::= (<specialization> ...)
       ;; <specialization> ::= (<argument-predicates> . <specialized-procedure>)
       ;; <argument-predicates> ::= (<predicate> ...)
       ;;                         | (<predicate> <predicate> ... . <rest-args-predicate>)
       ;;                         | <rest-args-predicate>
       ;; <predicate> ::= One-argument function which returns true or #f.
       ;; <rest-args-predicate> ::= Any-number-of-arguments function
       ;;                            which returns true or #f.
       ;; <specialized-procedure> ::= Procedure with arity matching <argument-predicates>
       ;;                             which returns any number and type of values
       (letrec ([specializations
                 ;; The specialization for configure must
                 ;; always be first so that other specializations
                 ;; can never precede it so a generic can reliably
                 ;; always be reconfigured.
                 (list (cons configure-preds
                             (lambda (ignore proc)
                               (set! specializations 
                                     (cons (car specializations)
                                           (proc (cdr specializations)))))))])
         (lambda args
           (let ([proc
                  (let next-spec ([specs specializations] [args args])
                    (cond 
                      [(pair? specs)
                       (let next-pred ([preds (caar specs)] [test-args args] [args args])
                         (cond 
                           [(pair? preds)
                            (if (and (pair? test-args)
                                     ((car preds) (car test-args)))
                              (next-pred (cdr preds) (cdr test-args) args)
                              (next-spec (cdr specs) args))]
                           [(null? preds)
                            (if (null? test-args)
                              (cdar specs)  ;; found the right specialized procedure
                              (next-spec (cdr specs) args))]
                           [else  ;; predicate for rest args
                            (if (apply preds test-args)
                              (cdar specs)  ;; found the right specialized procedure
                              (next-spec (cdr specs) args))]))]
                      [(null? specs) 
                       (apply assertion-violation who "no specialization" args)]))])
             (apply proc args))))]))
  
  (define-syntax define-generic
    ;;; The syntax of `define-generic' is similar to that of `case-lambda'.
    ;;; Each clause specifies a specialization for the generic.
    ;;;
    ;;; (define-generic <identifier> <spec-clause> ...)    syntax
    ;;;
    ;;; <spec-clause> ::= (<predicate-formals> . <body>)
    ;;; <predicate-formals> ::= ((<identifier> <predicate>) ...)
    ;;;                       | ((<identifier> <predicate>)
    ;;;                          (<identifier> <predicate>) ...
    ;;;                          . #(<identifier> <rest-predicate>))
    ;;;                       | #(<identifier> <rest-predicate>)
    ;;; <predicate> ::= Expression which evaluates to a one-argument
    ;;;                 function that returns true or #f.
    ;;; <rest-predicate> ::= Expression which evaluates to an
    ;;;                      any-number-of-arguments function that
    ;;;                      returns true or #f.
    (lambda (stx)
      (syntax-case stx ()
        [(_ name [pred-frmls . b] ...)
         (identifier? #'name)
         (with-syntax ([((preds frmls) ...) 
                        (map (lambda (pf)
                               (syntax-case pf ()
                                 [([a p] ...)
                                  #'((list p ...) (a ...))]
                                 [([a p] ... . #(ar pr))
                                  #'((cons* p ... pr) (a ... . ar))]))
                             #'(pred-frmls ...))])
           #'(define name 
               (let ([g (make-generic 'name)])                   
                 (specialize g preds (lambda frmls . b))
                 ...
                 g)))])))
  
)
