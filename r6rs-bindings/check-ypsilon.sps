#!/usr/bin/env ypsilon
#!r6rs

; By Derick Eddington.
; Placed in the public domain.

(import 
  (rnrs)
  (xitomatl r6rs-bindings check)
  (only (xitomatl common-unstandard) printf))

;;; NOTE: This will need to change if/when Ypsilon changes what exception(s)
;;;       and/or condition(s) are used.

(define undefined-msg-prefixes 
  '("attempt to reference unbound identifier"
    "attempt to reference unbound variable"))
(define syntax-misuse-msg-prefixes 
  '("misplaced syntactic keyword" "misplaced auxiliary syntactic keyword"
    "invalid syntax" "invalid buffer mode"
    "invalid eol style" "invalid directive" "invalid file options"))

(print-checks 
 (run-checks (lambda (exception)
               #;(begin (display "\n*** Got exception:\n")
                      (write exception) (newline))
               (cond
                 [(undefined-violation? exception)
                  #f]
                 [(exists 
                    (lambda (prefix)
                      (and (>= (string-length (condition-message exception))
                               (string-length prefix)) 
                           (string=? (substring (condition-message exception)
                                                0 (string-length prefix))
                                     prefix)))
                    undefined-msg-prefixes)
                  (printf "\nMissing &undefined: ~s\n" (simple-conditions exception))
                  #f]
                 [(and (syntax-violation? exception)
                       (exists
                         (lambda (prefix)
                           (and (>= (string-length (condition-message exception))
                                    (string-length prefix))
                                (string=? (substring (condition-message exception)
                                                     0 (string-length prefix))
                                          prefix)))
                         syntax-misuse-msg-prefixes))
                  #t]
                 [(and (who-condition? exception)
                       (eq? 'import (condition-who exception)))
                  (printf "\nProblem importing:\n~s\n" (simple-conditions exception))
                  #f]
                 [else
                  (printf "\nDon't know:\n~s\n" (simple-conditions exception))
                  (raise exception)]))))
