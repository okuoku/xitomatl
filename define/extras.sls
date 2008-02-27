;;; Provides define-values, define which does procedure currying,
;;; and define-syntax which does procedure style.

(library (xitomatl define extras)
  (export 
    (rename
      (my:define-values define-values)
      (my:define define)
      (my:define-syntax define-syntax)))
  (import 
    (rnrs)
    (only (xitomatl macro-utils) unique-ids?/raise)
    (only (xitomatl common-unstandard) format))
  
  (define (define-values-error expected received-vals)
    (apply assertion-violation 'define-values
      (format "expected ~a values, received ~a values" expected (length received-vals))
      received-vals))
  
  (define-syntax my:define-values
    ;; NOTE: When Bug #162785 is fixed, the t*s won't be necessary,
    ;;       and id*s can be defined in front of dummy and set! from the case-lambda.
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id* ...) expr)
         (and (for-all identifier? #'(id* ...)) 
              (unique-ids?/raise #'(id* ...) stx))
         (with-syntax ([(t* ...) (generate-temporaries #'(id* ...))])
           #`(begin
               (define t* #f) ...
               (define dummy 
                 (call-with-values 
                  (lambda () #f expr) ;; #f first to prevent internal defines
                  (case-lambda
                    [(id* ...)
                     (set! t* id*) ...
                     #f]
                    [otherwise 
                     (define-values-error #,(length #'(id* ...)) otherwise)])))
               (define id* t*) ...))])))
  

  (define-syntax my:define
    (syntax-rules ()
      [(_ ((maybe-list . f1) . f2) expr expr* ...)
       (my:define (maybe-list . f1)
         (lambda f2 expr expr* ...))]
      [(_ (name . formals) expr expr* ...)
       (define (name . formals) expr expr* ...)]
      [(_ name expr)
       (define name expr)]
      [(_ name)
       (define name #f)]))  ;; Remember, it's unspecified

    
  (define-syntax my:define-syntax
    (syntax-rules ()
      [(_ (name . args) expr expr* ...)
       (define-syntax name
         (lambda args expr expr* ...))]
      [(_ name expr)
       (define-syntax name expr)]))
  
)
