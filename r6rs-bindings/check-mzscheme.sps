#!/usr/bin/env mzscheme
#!r6rs

; By Derick Eddington.
; Placed in the public domain.

(import 
  (rnrs)
  (xitomatl r6rs-bindings check))

;;; NOTE: This will need to change if/when MzScheme changes what exception(s)
;;;       and/or condition(s) are used.

(print-checks 
 (run-checks (lambda (exception)
               #;(begin (display "\n*** Got exception:\n")
                      (write exception) (newline))
               (cond
                 [(syntax-violation? exception)
                  #t]
                 [(undefined-violation? exception)
                  #f]
                 [else
                  (error (car (command-line)) "don't know" exception)]))))
