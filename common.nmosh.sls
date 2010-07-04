;; based on ypsilon one

;; [[ORIGINAL COPYRIGHT STATEMENT]]
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time

    ; do not support them (currently)
    ;with-input-from-string with-output-to-string
    ;system
    ;; TODO: add to as needed/appropriate
    )
  (import
    (rnrs)
    (nmosh gensym)
    (mosh pp)
    (only (mosh) time format))
  
  (define (add1 x) (+ x 1))

  (define (sub1 x) (- x 1))
  
  (define (fprintf port fmt-str . fmt-args)
    (put-string port (apply format fmt-str fmt-args)))
  
  (define (printf fmt-str . fmt-args)
    (apply fprintf (current-output-port) fmt-str fmt-args))

#|
  (define (parameterize-current-port port set-port! val thunk)
    (define (swap)
      (let ((t (port)))
        (set-port! val)
        (set! val t)))
    (dynamic-wind swap thunk swap))
  
  (define (with-input-from-string str thunk)
    (parameterize-current-port current-input-port set-current-input-port!
                               (open-string-input-port str) thunk))

  (define (with-output-to-string thunk)
    (let-values (((sop get) (open-string-output-port)))
      (parameterize-current-port current-output-port set-current-output-port!
                                 sop thunk)
      (get)))
|#
)
