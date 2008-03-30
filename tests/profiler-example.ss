#!/usr/bin/env scheme-script
#!r6rs
(import
  (rnrs)
  (xitomatl profiler srfi-time))

(define/profiled (f . args) 
  (apply values args))
(define k0) 
(define k1)
(define/profiled (fk c? r?) 
  (when c? 
    (call/cc (lambda (cc) (set! k0 cc))))
  (if r? 
    'return
    (k1 'non-local-exit)))
((lambda/profiled () 
   (call/cc (lambda (k) (set! k1 k)))))

(f 1)
(f 2 2)
(f)
(f 5 5 5 5 5)
(fk #f #t)
(fk #t #t)
(k0)
(k0)
(fk #t #f)
(k0)
(k0)

(print-report)
