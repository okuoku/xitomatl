; By Derick Eddington.
; Placed in the public domain.

(import
  (rnrs)
  (rnrs eval)
  (rnrs eval reflection))

(define (get-bindings env-spec)
  (environment-bindings (environment env-spec)))

(define libspecs
  (call-with-input-file "all-r6rs-libspecs.s-expr" read))

(write (map (lambda (ls) (cons ls (get-bindings ls)))
            libspecs))
