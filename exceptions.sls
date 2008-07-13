#!r6rs
(library (xitomatl exceptions)
  (export
    catch
    warning)
  (import
    (rnrs)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand))
  
  (define-syntax catch
    (lambda (stx)
      (syntax-case stx ()
        [(_ var (in-clause ...) thunk-expr)
         (with-syntax* 
             ([catch-k (gen-temp)]
              [(out-clause ...) 
               (map (lambda (ic)
                      (syntax-case ic (=>)
                        [(test => proc) 
                         #'(test => (lambda (t) (catch-k (lambda () (proc t)))))]
                        [(test)
                         #'(test => (lambda (t) (catch-k (lambda () t))))]
                        [(test/else expr ...)
                         #'(test/else (catch-k (lambda () expr ...)))]))
                    #'(in-clause ...))])
           #`((call/cc
                (lambda (catch-k)
                  (lambda ()
                    (with-exception-handler
                      (lambda (var)
                        #,(and (positive? (length #'(out-clause ...)))
                               #'(cond out-clause ...))
                        (raise-continuable var))
                      thunk-expr))))))])))
  
  (define (warning who msg . irrts)
    (raise-continuable
      (condition
        (make-warning)
        (if who 
          (make-who-condition who)
          (condition))
        (make-message-condition msg)
        (if (positive? (length irrts))
          (make-irritants-condition irrts)
          (condition)))))    
)
