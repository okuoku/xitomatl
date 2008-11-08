#!r6rs
(library (xitomatl exceptions)
  (export
    catch
    warning assertion-violation/conditions error/conditions
    print-exception)
  (import
    (rnrs)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand)
    (only (xitomatl conditions) print-condition)
    (only (xitomatl common-unstandard) pretty-print))
  
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
                        ;; If R7RS makes exceptions discernable as continuable or not,
                        ;; this will change to use a `reraise' which will discern and
                        ;; use raise or raise-continuable depending.
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
  
  (define (assertion-violation/conditions who msg irrts . cndts)
    (raise 
     (apply condition
            (make-assertion-violation)
            (make-who-condition who)
            (make-message-condition msg)
            (make-irritants-condition irrts)
            cndts)))
  
  (define (error/conditions who msg irrts . cndts)
    (raise 
     (apply condition
            (make-error)
            (make-who-condition who)
            (make-message-condition msg)
            (make-irritants-condition irrts)
            cndts)))
  
  (define print-exception 
    (case-lambda
      [(exn)
       (print-exception exn (current-output-port))]
      [(exn p)
       (display "Exception:\n" p)
       (if (condition? exn)
         (print-condition exn p)
         (pretty-print exn p))]))
)
