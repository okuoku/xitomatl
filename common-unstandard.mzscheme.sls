#!r6rs
(library (xitomatl common-unstandard)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time current-milliseconds
    ;; TODO: add to as needed/appropriate
    )
  (import
    (rnrs)
    (only (scheme) add1 sub1 format printf fprintf gensym time current-milliseconds)
    (only (scheme pretty) pretty-print))
)
