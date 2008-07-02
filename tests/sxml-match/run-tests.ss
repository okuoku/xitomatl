#!/usr/bin/env scheme-script
#!r6rs
(import
  (rnrs)
  (xitomatl sxml-match)
  (xitomatl include)
  (xitomatl srfi lightweight-testing)
  (only (xitomatl common-unstandard) printf))

  (define-syntax module
    (syntax-rules ()
      [(_ _ _ . r) (begin . r)]))
  
  (define-syntax require
    (syntax-rules ()
      [(_ . _) (begin)]))
  
  (define-syntax run-test
    (syntax-rules ()
      [(_ desc test expected-result)
       (begin
         (printf "\nRunning: ~a:\n" desc)
         (check test => expected-result))]))

(include/resolve ("xitomatl" "tests" "sxml-match") "sxml-match-tests.ss")

(check-report)
