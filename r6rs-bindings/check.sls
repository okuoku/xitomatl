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
    (xitomatl r6rs-bindings helpers))
  
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
                            (eval (binding-name bs) 
                                  (if (list? lib)
                                    (environment lib)
                                    (case lib
                                      [(null-environment)
                                       (null-environment 5)]
                                      [(scheme-report-environment)
                                       (scheme-report-environment 5)]
                                      [else
                                       (error 'run-checks "unknown non-list lib" lib)])))
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
       (define (d x) (display x port))
       (define (nl) (newline port))
       (for-each (lambda (lib+lib-results)
                   (define lib (car lib+lib-results))
                   (define total (caadr lib+lib-results))
                   (define missing (cdadr lib+lib-results))
                   (define lib-results (cddr lib+lib-results))
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
                               (define present (car r))
                               (define bs (cdr r))
                               (unless present
                                 (d "  ")
                                 (d (binding-name bs))
                                 (nl)))
                             lib-results))
                 results)
       
#|       (let* ([rem-dups
               (lambda (l)
                 (let loop ([l l] [n '()])
                   (if (null? l)
                     n
                     (loop (remove (car l) (cdr l))
                           (cons (car l) n)) )))]
              [all-ids
               (rem-dups (apply append 
                                (map (lambda (r)
                                       (map (lambda (p+bs) (binding-name (cdr p+bs))) 
                                            (cddr r))) 
                                     results)))]
              [lib+ids*
               ; ([<lib-name> <identifier> ...] ...)
               (map (lambda (r)
                      (cons (car r)
                            (map (lambda (p+bs) (binding-name (cdr p+bs))) 
                                 (cddr r))))
                    results)]
              [count
               (lambda (x l)
                 (fold-left (lambda (c e) (if (equal? e x) (+ 1 c) c)) 0 l))]
              [id-counts
               ; ([<identifier> (<lib-name> . <count>) ...] ...)
               (map (lambda (id)
                      (cons id
                            (map (lambda (lib+ids)
                                   (cons (car lib+ids) (count id (cdr lib+ids))))
                                 lib+ids*)))
                    all-ids)]
              [dup-ids
               ; ([<identifier> (<lib-name> . <count>) ...] ...)
               (filter (lambda (id+c) 
                         (< 1 (length (cdr id+c))))
                       (map (lambda (id+c)
                              (cons (car id+c)
                                    (filter (lambda (ln+c) (positive? (cdr ln+c)))
                                            (cdr id+c))))
                            id-counts))])
         
         (d "\n------------------------------------------------------\n")
         (d "Identifiers occuring in more than one library,\n")
         (d "excluding those occuring in only their origin library\n")
         (d "and the composite (rnrs (6)):\n")
         (d "------------------------------------------------------\n")
         (if (positive? (length dup-ids))
           (for-each (lambda (id+c)
                       (d " ")
                       (d (car id+c))
                       (nl)
                       (for-each (lambda (ln+c)
                                   (d "  ")
                                   (d (car ln+c))
                                   (nl))
                                 (cdr id+c)))
                     (list-sort (lambda (id+c-1 id+c-2)
                                  (string<? (symbol->string (car id+c-1)) 
                                            (symbol->string (car id+c-2))))
                                (filter (lambda (id+c)
                                          (< 1 (length (remp (lambda (ln+c)
                                                               (equal? '(rnrs (6)) (car ln+c))) 
                                                             (cdr id+c)))))
                                        dup-ids)))
           (d " None.\n")))
|#       
       (let-values ([(all-total all-missing)
                     (let ([at+am
                            (fold-left (lambda (a t+m) (cons (+ (car a) (car t+m)) 
                                                             (+ (cdr a) (cdr t+m))))
                                       '(0 . 0)
                                       (map cadr results))])
                       (values (car at+am) (cdr at+am)))])
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
