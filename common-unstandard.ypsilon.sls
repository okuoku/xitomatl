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
    (rnrs)
    (core))
  
  (define (add1 x) (+ x 1))

  (define (sub1 x) (- x 1))
  
  (define (fprintf port fmt-str . fmt-args)
    (display (apply format fmt-str fmt-args) port))
  
  (define (printf fmt-str . fmt-args)
    (apply fprintf (current-output-port) fmt-str fmt-args))
  
  (define-syntax time
    (lambda (stx)
      (syntax-violation #f "not implemented" stx)))
  
  (define (current-milliseconds)
    ;; Returns: fixnum of the "current" millisecond, the reference point isn't
    ;; known, and so should only be used for calculating a difference between 
    ;; other values returned by this procedure, within a reasonable time span 
    ;; before the fixnum overflows/wraps. 
    (assertion-violation 'current-milliseconds "not implemented"))
  
  (define (with-output-to-string . args)
    (error 'with-output-to-string "not available from this implementation"))
)
