; By Derick Eddington.
; Placed in the public domain.

(library (r6rs-bindings helpers)
  (export
    all-bindings-spec-filename
    read-all-bindings-specs
    binding-name
    binding-type
    binding-from
    binding-levels
    only-variables
    only-macros
    all-libs
    all-ids)
  (import 
    (rnrs))
  
  ;;; The bindings specification file is expected to contain one list of the shape:
  ;;; ((<library-name>
  ;;;   [(name <binding-identifier>) 
  ;;;    (type < 'variable or 'macro >) 
  ;;;    (from <library-name>)
  ;;;    (levels (<non-negative-integer> ...))] 
  ;;;   ...)
  ;;;  ...)
  
  (define all-bindings-spec-filename "all-r6rs-bindings-from-larceny.s-expr")

  (define read-all-bindings-specs
    (case-lambda
      [() (read-all-bindings-specs all-bindings-spec-filename)]
      [(filename) (call-with-input-file filename read)]))
  
  (define (binding-name bs) (cadar bs))
  
  (define (binding-type bs) (cadadr bs))
  
  (define (binding-from bs) (cadr (caddr bs)))
  
  (define (binding-levels bs) (cadar (cdddr bs)))
  
  (define (only-variables bindings-specs)
    (filter (lambda (bs) (equal? 'variable (binding-type bs))) 
            bindings-specs))
  
  (define (only-macros bindings-specs)
    (filter (lambda (bs) (equal? 'macro (binding-type bs))) 
            bindings-specs))
  
  (define (all-libs)
    (map car (read-all-bindings-specs)))
  
  (define (rem-dups l)
    (let loop ([l l] [n '()])
      (if (null? l)
        n
        (loop (remove (car l) (cdr l))
              (cons (car l) n)) )))
  
  (define (all-ids)
    (list-sort (lambda (id1 id2) (string<? (symbol->string id1)
                                           (symbol->string id2)))
               (rem-dups (apply append 
                                (map (lambda (ln+bs) (map binding-name (cdr ln+bs))) 
                                     (read-all-bindings-specs))))))
  
)
