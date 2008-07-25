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
    (ikarus))
  
  (define current-milliseconds
    ;; Returns: fixnum of the "current" millisecond, the reference point isn't
    ;; known, and so should only be used for calculating a difference between 
    ;; other values returned by this procedure, within a reasonable time span 
    ;; before the fixnum overflows/wraps. 
    (let ([m (fxdiv (greatest-fixnum) 1000)])
      (define-struct my-time (msecs secs usecs))
      (lambda ()
        (let ([t (foreign-call "ikrt_current_time" (make-my-time 0 0 0))])
          ;; This trick avoids going into bignum range.
          (fx+ (fx* (fxmod (my-time-secs t) m) 1000) 
               (fxdiv (my-time-usecs t) 1000))))))
)
