#!r6rs
(import
  (rnrs)
  (xitomatl ports)
  (xitomatl smatch))

(define top-level-names
  (case-lambda
    [(input)
     (top-level-names input "define" "define-syntax")]
    [(input . look-for)
     (let ([look-for (map string->symbol look-for)])
       (for-each
         (smatch-lambda
           [(f (n . r) . b) 
            (and (symbol? f) (memq f look-for) (symbol? n))
            (begin (display n) (newline))]
           [(f n . r) 
            (and (symbol? f) (memq f look-for) (symbol? n))
            (begin (display n) (newline))]
           [_ #f])
         (call-with-input-file input read-all)))]
    [args
     (assertion-violation (car (command-line)) 
       "invalid command line arguments" args)]))

(apply top-level-names (cdr (command-line)))
