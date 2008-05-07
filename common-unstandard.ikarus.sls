#!r6rs
(library (xitomatl common-unstandard)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    make-list last-pair
    time current-milliseconds
    ;; TODO: add to as needed/appropriate
    )
  (import
    (ikarus))
  
  (define current-milliseconds
    (let ([m (fxdiv (greatest-fixnum) 1000)])
      (define-struct my-time (msecs secs usecs))
      (lambda ()
        (let ([t (foreign-call "ikrt_current_time" (make-my-time 0 0 0))])
          (fx+ (fx* (fxmod (my-time-secs t) m) 1000) 
               (fxdiv (my-time-usecs t) 1000))))))
)
