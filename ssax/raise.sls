;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl ssax raise)
  (export
    parser-error
    ssax:warn)
  (import
    (rnrs)
    (only (xitomatl conditions) make-port-position-condition))
  
  (define (make-f raise-it first who)
    (lambda (port msg . other-msg)
      (raise-it
       (condition
        first
        (make-who-condition who)
        (make-message-condition (call-with-string-output-port
                                 (lambda (sop)
                                   (for-each (lambda (x) (display x sop))
                                             (cons msg other-msg)))))
        (if (and (port? port) (port-has-port-position? port))
          (make-port-position-condition (port-position port))
          (condition))
        (make-irritants-condition (list port))))))
   
  (define parser-error
    (make-f raise (make-error) 'ssax:parser))
   
  (define ssax:warn
    ;; None of these condition types are &serious,
    ;; so a default exception handler should return (per the R6RS),
    ;; allowing the SSAX code which called this to continue.
    (make-f raise-continuable (make-warning) 'ssax))
  
)
