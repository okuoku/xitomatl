#!r6rs
(library (xitomatl common-unstandard)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time current-milliseconds
    with-output-to-string
    ;; TODO: add to as needed/appropriate
    )
  (import
    (except (rnrs) current-output-port)
    (only (scheme) add1 sub1 format printf fprintf gensym time current-milliseconds
                   current-output-port parameterize)
    (only (scheme pretty) pretty-print))
  
  (define (with-output-to-string thunk)
    (let-values ([(sop get) (open-string-output-port)])
      (parameterize ([current-output-port sop])
        (thunk))
      (get)))
)
