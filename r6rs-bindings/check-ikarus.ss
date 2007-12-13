#!/usr/bin/env scheme-script

; By Derick Eddington.
; Placed in the public domain.

(import 
  (rnrs)
  (r6rs-bindings check))

;;; NOTE: This will need to change if/when Ikarus changes what exception(s)
;;;       and/or condition(s) or used.

(print-checks 
 (run-checks (lambda (exception)
               (define msg (condition-message exception))
               (cond
                 [(exists (lambda (s) (string=? s msg))
                          '("unbound identifier"
                            "primitive not supported yet"))
                  #f]
                 [(exists (lambda (s) (string=? s msg))
                          '("invalid expression"
                            "invalid syntax"
                            "incorrect usage of auxiliary keyword"))
                  #t]
                 [else
                  (raise exception)]))))
