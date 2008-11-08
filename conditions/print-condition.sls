#!r6rs
(library (xitomatl conditions print-condition)
  (export
    print-condition)
  (import
    (rnrs)
    (only (xitomatl common-unstandard) pretty-print))

  (define print-condition
    (case-lambda
      [(c)
       (print-condition c (current-output-port))]
      [(c p)
       ;; TODO: Nicely formated print-out like Ikarus does
       (display "Condition:\n" p)
       (pretty-print (simple-conditions c) p)]))
)
