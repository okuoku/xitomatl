; By Derick Eddington.
; Placed in the public domain.

(import
  (rnrs)
  (only (rnrs r5rs) null-environment scheme-report-environment)
  (rnrs eval)
  (rnrs eval reflection))

(define (get-bindings env-spec)
  (environment-bindings 
    (if (list? env-spec)
      (environment env-spec)
      (case env-spec
        [(null-environment)
         (null-environment 5)]
        [(scheme-report-environment)
         (scheme-report-environment 5)]
        [else
         (error 'get-bindings "unknown non-list env-spec" env-spec)]))))

(define libspecs
  (call-with-input-file "all-r6rs-libspecs.s-expr" read))

(write (map (lambda (ls) (cons ls (get-bindings ls)))
            libspecs))
