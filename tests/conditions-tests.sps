#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl conditions))

(define c 
  (condition (make-argument-name-condition 'foo)
             (make-predicate-expression-condition 'thing?)
             (make-port-position-condition 123)))

(check (catch ex ((else (and (argument-name-condition? ex)
                             (predicate-expression-condition? ex)
                             (port-position-condition? ex)
                             (list (condition-argument-name ex)
                                   (condition-predicate-expression ex)
                                   (condition-port-position ex)))))
         (raise c))
       => '(foo thing? 123))

(let-values (((sop get) (open-string-output-port)))
  (print-condition c sop)
  (let ((s (get)))
    (check (positive? (string-length s)) => #T)
    (check (string-ref s (- (string-length s) 1)) => #\newline)))


(check-report)
