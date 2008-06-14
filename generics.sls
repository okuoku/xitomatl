#!r6rs
(library (xitomatl generics)
  (export
    make-generic define-generic specialize)
  (import
    (rnrs))

  ;;; Generics are procedures which delegate to some underlying procedure 
  ;;; determined by the arguments to the generic.  A per-generic association
  ;;; of predicates to underlying-procedures is used to determine and find 
  ;;; what underlying to use.
  
  ;;; This special distinct object, only available inside this library,
  ;;; is used to (re)configure the specializations of a generic.  It is
  ;;; used for adding a specialization and it could be used for possible
  ;;; future functionality like removing a specialization.
  (define-record-type configure)
  (define C (make-configure))
  
  (define (specialize generic preds proc)
    (define who 'specialize)
    (unless (procedure? generic)
      (assertion-violation who "not a generic" generic))
    (unless (valid-predicates-list? preds)
      (assertion-violation who "not a valid specialization predicates list" preds))
    (unless (procedure? proc)
      (assertion-violation who "not a procedure" proc))
    ;; append is used so that specializations have precedence according
    ;; to the order they were added.
    (generic C (lambda (specializations) 
                 (append specializations (list (cons preds proc))))))
  
  (define (valid-predicates-list? x)
    (cond [(pair? x) (and (procedure? (car x)) (valid-predicates-list? (cdr x)))]
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
       ;;                         | (<predicate> <predicate> ... . <rest-args-predicates>)
       ;;                         | <rest-args-predicates>
       ;; <predicate> ::= One argument procedure which returns a boolean.
       ;; <rest-args-predicates> ::= Any number of arguments procedure 
       ;;                            which returns a boolean.
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
    (lambda (stx)
      (syntax-case stx ()
        [(_ name [pred-frmls . b] ...)
         (with-syntax ([((preds frmls) ...) 
                        (map (lambda (pf)
                               (syntax-case pf (R)
                                 [([a p] ...)
                                  #'((list p ...) (a ...))]
                                 [([a0 p0] [a p] ... [R ar pr])
                                  #'((cons* p0 p ... pr) (a0 a ... . ar))]
                                 [[R ar pr]
                                  #'(pr ar)]))
                             #'(pred-frmls ...))])
           #'(begin
               (define name (make-generic 'name))
               (specialize name preds (lambda frmls . b))
               ...))])))
  
)