#!r6rs
(library (xitomatl coroutines)
  (export
    make-coroutine
    coroutine
    case-coroutine
    define-coroutine
    coroutine-finished-condition?
    condition-finished-coroutine)
  (import
    (rnrs)
    (only (xitomatl define extras) define/?/AV))
  
  ;;; Inspired by Will Farr's generators example:
  ;;; http://wmfarr.blogspot.com/2006/08/one-more-example-of-python-generators.html
  ;;; 
  ;;; NOTE: These coroutines are not thread/engine safe,
  ;;;       and a coroutine must not call itself.
  
  (define-condition-type &coroutine-finished &condition
    make-coroutine-finished-condition coroutine-finished-condition?
    (coroutine condition-finished-coroutine))
  
  (define/?/AV (make-coroutine [make-proc procedure?])
    (letrec* 
        ([yield 
          (lambda args
            (call/cc
              (lambda (k)
                (set! resume k)
                (return (lambda () (apply values args))))))]
         [resume 
          (let ([proc (make-proc yield)])
            (unless (procedure? proc)
              (AV "make-proc did not return a procedure" proc))
            ;; The initial `resume' isn't actually resuming.  It's what starts
            ;; the coroutine by calling the procedure which contains the body
            ;; of the coroutine.
            (lambda args
              (with-exception-handler
                (lambda (ex)
                  ;; Doing this makes the dynamic environment (e.g. the
                  ;; exception handlers) of the raise of the exception 
                  ;; from inside proc be that of the current invocation of
                  ;; the coroutine.  Otherwise, it would always be that
                  ;; of the first invocation.
                  (return (lambda () (raise ex))))
                (lambda () (apply proc args)))
              (let ([cf (make-coroutine-finished-condition coroutine)])
                ;; Set resume to this so that proc is not re-entered if the
                ;; coroutine is invoked again after proc has returned.
                (set! resume (lambda args (return (lambda () (raise cf)))))
                ;; Raise in the dynamic environment of the current
                ;; invocation of the coroutine.
                (return (lambda () (raise cf))))))]
         [return #f]
         [coroutine
          (lambda args
            (when return
              (assertion-violation 'coroutine 
                "illegal recursive or concurrent call" coroutine))
            (let ([return-proc (call/cc
                                 (lambda (k)
                                   (set! return k)
                                   (apply resume args)))])
              (set! return #f)
              (return-proc)))])
      coroutine))
  
  (define-syntax case-coroutine/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt [frmls b0 b ...] ...)
         (with-syntax ([yield (datum->syntax #'ctxt 'yield)])
           #'(make-coroutine
               (lambda (yield)
                 (case-lambda [frmls b0 b ...] ...))))])))
  
  ;;; NOTE: Matching arguments and selecting a clause only happens
  ;;        the first time the coroutine is called.
  (define-syntax case-coroutine
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt [frmls b0 b ...] ...)
         #'(case-coroutine/lexical-context ctxt [frmls b0 b ...] ...)])))
  
  (define-syntax coroutine
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt frmls b0 b ...)
         #'(case-coroutine/lexical-context ctxt [frmls b0 b ...])])))
  
  (define-syntax define-coroutine
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name . frmls) b0 b ...)
         (identifier? #'name)
         #'(define name
             (case-coroutine/lexical-context name [frmls b0 b ...]))])))

)

#|
(define-coroutine (g n)
  (do ([i 0 (+ 1 i)])
    [(= i n)
     (display "g finished\n")]
    (set! i (apply yield (make-list i 'x)))))

(define-syntax python-generator
  (lambda (stx)
    (syntax-case stx ()
      [(ctxt frmls b0 b ...)
       #'(lambda frmls
           (case-coroutine/lexical-context ctxt [() b0 b ...]))])))

(define-syntax define-python-generator
  (lambda (stx)
    (syntax-case stx ()
      [(ctxt (name . frmls) b0 b...)
       #'(define (name . frmls)
           (case-coroutine/lexical-context ctxt [() b0 b ...]))])))
|#
