#!r6rs
;; Copyright (c) 2010 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (only (rnrs r5rs) quotient)
  (only (srfi :13 strings) string-concatenate-reverse)
  (only (srfi :19 time) current-time time-second time-nanosecond)
  (only (xitomatl include) include/resolve)
  (only (xitomatl strings) string-split)
  (only (xitomatl tests irregex test) use)
  (xitomatl irregex))

(define-syntax import (syntax-rules () ((_ . _) (begin))))
(define (nth-value _ x) x)
(define string-match irregex-match)
(define read-line get-line)

(define (cpu-time) ; Not actually the CPU time, it's the total time.
  (let ((x (current-time)))
    (* 1e3 (+ (time-second x) (/ (time-nanosecond x) 1e9)))))

(include/resolve ("xitomatl" "tests" "irregex") "run-bench.scm")
