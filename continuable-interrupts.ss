(library (continuable-interrupts)
  (export 
    with-continuable-interrupts
    with-continuable-interrupts*
    with-continuable-interrupts/top-level)
  (import (ikarus))
  
  (define-syntax with-continuable-interrupts
    ;;; esc is given the continuation of the interrupt handler, 
    ;;; which is the continuation to resume what was interrupted.
    (syntax-rules ()
      [(_ esc thunk)
       (let ([t thunk])
         (unless (procedure? t)
           (assertion-violation 'with-continuable-interrupts "not a procedure" t))
         (parameterize 
             ([interrupt-handler (lambda () (call/cc (lambda (k) (esc k))))])
           (t)))]))

  (define-syntax with-continuable-interrupts*
    ;;; Only works where a (begin ...) with internal defines can work.
    (syntax-rules ()
      [(wci* int-k-name thunk)
       (wci* int-k-name thunk void)]
      [(_ int-k-name thunk indicate-interrupted)
       (begin
         (define int-k-name (void))
         (let ([ii indicate-interrupted]) 
           (unless (procedure? ii)
             (assertion-violation 'with-continuable-interrupts* "not a procedure" ii))
           (call-with-values
            (lambda () 
              (call/cc (lambda (k) 
                         (values (with-continuable-interrupts k thunk)
                                 #t))))
            (case-lambda
              [(int-k) 
               (set! int-k-name int-k)
               (ii)]
              [(thunk-result flag)
               (set! int-k-name (void))
               thunk-result]))))]))
  
  (define-syntax with-continuable-interrupts/top-level
    ;;; Only works where a (begin ...) with internal defines can work.
    (syntax-rules ()
      [(_ int-k-name thunk)
       (with-continuable-interrupts* int-k-name thunk 
                                     (lambda () (display "Back to top-level.\n")))]))
)
