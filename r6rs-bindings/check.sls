; By Derick Eddington.
; Placed in the public domain.
#!r6rs
(library (xitomatl r6rs-bindings check)
  (export
    run-checks
    print-checks)
  (import 
    (rnrs)
    (only (rnrs r5rs) null-environment scheme-report-environment)
    (rnrs eval)
    (xitomatl r6rs-bindings utils))
  
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
    (map (lambda (lib)
           (define ids (identifiers-of lib))
           (define total (length ids))
           (define missing 0)
           (define results
             (begin 
               #|(display "\nChecking library ") (display lib) (newline)
               (display "------------------------------------------------------\n")|#
               (map (lambda (id)
                      (guard (ex [#t 
                                  ;; Here we give the exception to client-supplied 
                                  ;; present? so it can tell us if the exception
                                  ;; means the binding is not present.
                                  (let ([p? (present? ex)])
                                    (unless p?
                                      (set! missing (+ 1 missing)))
                                    (cons p? id))])
                        (eval id 
                              (if (list? lib)
                                (environment lib)
                                (case lib
                                  [(null-environment)
                                   (null-environment 5)]
                                  [(scheme-report-environment)
                                   (scheme-report-environment 5)]
                                  [else
                                   (error 'run-checks "unknown non-list lib" lib)])))
                        (cons #t id)))
                    (list-sort 
                      (lambda (id1 id2) 
                        (string<? (symbol->string id1) (symbol->string id2)))
                      ids))))
           (vector lib total missing results))
         (all-libraries-names)))
  
  (define print-checks
    (case-lambda
      [(results) (print-checks results (current-output-port))]
      [(results port)
       (define (d x) (display x port))
       (define (nl) (newline port))
       (for-each (lambda (ltmr)
                   (define lib (vector-ref ltmr 0))
                   (define total (vector-ref ltmr 1))
                   (define missing (vector-ref ltmr 2))
                   (define lib-results (vector-ref ltmr 3))
                   (d "\n------------------------------------------------------\n")
                   (d "Library: ") (d lib) (nl)
                   (d "------------------------------------------------------\n")
                   (d " Present: ")
                   (d (/ (round (* (/ (- total missing) total) 10000)) 100.0))
                   (d "%\n")
                   (d " Missing: ")
                   (d missing) (d " out of ") (d total) 
                   (d ":\n") 
                   (for-each (lambda (r)
                               (unless (car r)
                                 (d "  ") (d (cdr r)) (nl)))
                             lib-results))
                 results)
       (let-values ([(all-total all-missing)
                     (let ([at.am
                            (fold-left (lambda (a t.m) (cons (+ (car a) (car t.m)) 
                                                             (+ (cdr a) (cdr t.m))))
                                       '(0 . 0)
                                       (map (lambda (r) (cons (vector-ref r 1) (vector-ref r 2))) 
                                            results))])
                       (values (car at.am) (cdr at.am)))])
         (d "\n-----------------------------------------------------------\n")
         (d "All libraries, counting multiple occurances of identifiers:\n")
         (d "-----------------------------------------------------------\n")
         (d " Present: ")
         (d (/ (round (* (/ (- all-total all-missing) all-total) 10000)) 100.0))
         (d "%\n")
         (d " Missing: ")
         (d all-missing) (d " out of ") (d all-total) 
         (nl))
       (nl)]))
  
)
