#!r6rs
(import
  (rnrs)
  (xitomatl srfi lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl conditions))

(define c 
  (condition (make-argument-name-condition 'foo)
             (make-predicate-condition 'thing?)
             (make-port-position-condition 123)))

(check (catch ex ([else (and (argument-name-condition? ex)
                             (predicate-condition? ex)
                             (port-position-condition? ex)
                             (list (condition-argument-name ex)
                                   (condition-pred ex)
                                   (condition-port-position ex)))])
         (raise c))
       => '(foo thing? 123))

(let-values ([(sop get) (open-string-output-port)])
  (print-condition c sop)
  (let ([s (get)])
    (check (positive? (string-length s)) => #T)
    (check (string-ref s (- (string-length s) 1)) => #\newline)))


(check-report)
