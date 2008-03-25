#!/usr/bin/env scheme-script
#!r6rs
(import 
  (rnrs) 
  (rnrs eval)
  (xitomatl keyword-args)
  (xitomatl conditions)
  (xitomatl srfi lightweight-testing))

(define-syntax check-raised
  (syntax-rules (=>)
    [(_ expr => pred)
     (check 
       (guard (ex [#t (pred ex)])  
         (eval 'expr (environment '(rnrs) '(xitomatl keyword-args))))
       => #t)]))

(define (missing-kw-arg? ex)
  (and (assertion-violation? ex)
       (who-condition? ex)
       (message-condition? ex)
       (string=? "missing required keyword argument" (condition-message ex))
       (argument-name-condition? ex)))

(define (unknown-kw-arg? ex)
  (and (assertion-violation? ex)
       (who-condition? ex)
       (message-condition? ex)
       (string=? "unknown keyword argument" (condition-message ex))))


;;;; Expand-time keyword argument processing

(define/kw/e (f0 a [b 123])
  (list a b))

(check (f0 [:- [a 'foo]]) => '(foo 123))
(check (f0 [:- [b "asdf"] [a 'bar]]) => '(bar "asdf"))
(check-raised (let ()
                (define/kw/e (f0 a [b 123])
                  (list a b))
                (f0)) 
  => missing-kw-arg?)
(check-raised (let ()
                (define/kw/e (f0 a [b 123])
                  (list a b))
                (f0 [:- [c 'hmm]])) 
  => missing-kw-arg?)

(check-raised (let ()
                (define/kw/e (f0 a [b 123])
                  (list a b))
                (f0 [:- [a 'foo] [c 'hmm]])) 
  => unknown-kw-arg?)


(define/kw/e (f1 [a 1] b c [d 2] [e 3] f)
  (list f e d c b a))

(check (f1 [:- [d 1] [f 2] [a 3] [e 4] [c 5] [b 6]]) 
       => '(2 4 1 5 6 3))
#;(check (f1 [:- [b 1] [c 2] [f 3] [b 4]]) => 'bork)


;; TODO: kw-rest tests


;; TODO: more extensive eval-time default argument values tests


;; TODO: define/kw/r tests



;;;; TODO: Run-time keyword argument processing



(check-report)
