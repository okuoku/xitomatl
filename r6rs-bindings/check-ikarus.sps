; By Derick Eddington.
; Placed in the public domain.

(import 
  (rnrs)
  (xitomatl r6rs-bindings check)
  (only (ikarus) printf))

;;; NOTE: This will need to change if/when Ikarus changes what exception(s)
;;;       and/or condition(s) are used.

(print-checks 
 (run-checks (lambda (exception)
               (define msg (condition-message exception))
               (cond
                 [(exists (lambda (s) (string=? s msg))
                          '("unbound identifier"
                            "primitive not supported yet"))
                  (unless (undefined-violation? exception)
                    (printf "Missing &undefined: ~s ~a\n" msg (condition-irritants exception)))
                  #f]
                 [(exists (lambda (s) (string=? s msg))
                          '("invalid expression"
                            "invalid syntax"
                            "incorrect usage of auxiliary keyword"))
                  (unless (syntax-violation? exception)
                    (printf "Missing &syntax: ~s ~a\n" msg (condition-irritants exception)))
                  #t]
                 [else
                  (raise exception)]))))
