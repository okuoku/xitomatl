; By Derick Eddington.
; Placed in the public domain.

(library (r6rs-bindings check)
  (export
    run-checks
    print-checks)
  (import 
    (rnrs)
    (rnrs eval)
    (r6rs-bindings helpers))
  
  (define (run-checks present?)
    ;;; present? must be a procedure which takes one argument. This argument
    ;;; is an exception raised by eval when a binding is not present in the 
    ;;; library-environment currently being tested or when a syntax-keyword
    ;;; which *is present* is incorrectly used because we are just eval'ing
    ;;; its identifier. present? is responsible for using the exception to
    ;;; return #t if the exception means the binding is present or
    ;;; to return #f if the exception means the binding is not present.
    ;;; If for some odd reason present? can't understand the exception,
    ;;; it must re-raise it so we can see that unhandled exception.
    (map (lambda (lib+bspecs)
           (define lib (car lib+bspecs))
           (define bspecs (cdr lib+bspecs))
           (define total (length bspecs))
           (define missing 0)
           (define lib-results
             (begin 
               #|(display "\nChecking library ") (display lib) (newline)
               (display "------------------------------------------------------\n")|#
               (map (lambda (bs)
                      ;;;(display (binding-name bs)) (newline)
                      (call/cc 
                       (lambda (k)
                         (with-exception-handler
                          (lambda (ex)
                            ;; Here we give the exception to client-supplied 
                            ;; present? so it can tell us if the exception
                            ;; means the binding is not present.
                            (let ([p? (present? ex)])
                              (unless p?
                                (set! missing (+ 1 missing)))
                              (k (cons p? bs))))
                          (lambda ()
                            (eval (binding-name bs) (environment lib))
                            (cons #t bs))))))
                    (list-sort 
                      (lambda (bs1 bs2) 
                        (string<? (symbol->string (binding-name bs1))
                                  (symbol->string (binding-name bs2))))
                      bspecs))))
           (cons* lib (cons total missing) lib-results))
         (read-all-bindings-specs)))
  
  (define print-checks
    (case-lambda
      [(results) (print-checks results (current-output-port))]
      [(results port)
       (for-each (lambda (lib+lib-results)
                   (define lib (car lib+lib-results))
                   (define total (caadr lib+lib-results))
                   (define missing (cdadr lib+lib-results))
                   (define lib-results (cddr lib+lib-results))
                   (display "\n------------------------------------------------------\n" port)
                   (display "Library: " port) (display lib port) (newline port)
                   (display "------------------------------------------------------\n" port)
                   (display " Present: " port)
                   (display (/ (round (* (/ (- total missing) total) 10000)) 100.0) port)
                   (display "%\n" port)
                   (display " Missing: " port)
                   (display missing port) (display " out of " port) (display total port) 
                   (display ":\n" port) 
                   (for-each (lambda (r)
                               (define present (car r))
                               (define bs (cdr r))
                               (unless present
                                 (display "  " port)
                                 (display (binding-name bs) port)
                                 (newline)))
                             lib-results))
                 results)
       (let-values ([(all-total all-missing)
                     (let ([at+am
                            (fold-left (lambda (a t+m) (cons (+ (car a) (car t+m)) 
                                                             (+ (cdr a) (cdr t+m))))
                                       '(0 . 0)
                                       (map cadr
                                            (remp (lambda (r) (equal? '(rnrs (6)) (car r))) 
                                                  results)))])
                       (values (car at+am) (cdr at+am)))])
         (display "\n------------------------------------------------------\n" port)
         (display "All libraries, not counting the composite (rnrs (6)):\n" port)
         (display "------------------------------------------------------\n" port)
         (display " Present: " port)
         (display (/ (round (* (/ (- all-total all-missing) all-total) 10000)) 100.0) port)
         (display "%\n" port)
         (display " Missing: " port)
         (display all-missing port) (display " out of " port) (display all-total port) 
         (newline port))]))
  
)