(library (continuable-interrupts)
  (export 
    with-continuable-interrupts
    with-continuable-interrupts*
    with-continuable-interrupts/top-level)
  (import (ikarus))
  
  (define-syntax with-continuable-interrupts
    (syntax-rules ()
      [(_ esc thunk)
       (parameterize 
           ([interrupt-handler (lambda () (call/cc (lambda (k) (esc k))))])
         (thunk))]))

  (define-syntax with-continuable-interrupts*
    (syntax-rules ()
      [(wci* int-k-name thunk)
       (wci* int-k-name thunk (void))]
      [(_ int-k-name thunk indicate-interrupted)
       (begin
         (define int-k-name (void))
         (call-with-values
          (lambda () (call/cc (lambda (k) 
                                (values (with-continuable-interrupts k thunk)
                                        #t))))
          (case-lambda
            [(int-k) 
             (set! int-k-name int-k)
             indicate-interrupted]
            [(thunk-result flag)
             (set! int-k-name (void))
             thunk-result])))]))
  
  (define-syntax with-continuable-interrupts/top-level
    (syntax-rules ()
      [(_ int-k-name thunk)
       (with-continuable-interrupts* int-k-name thunk (display "Back to top-level.\n"))]))
)
