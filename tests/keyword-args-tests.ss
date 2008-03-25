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
(check (f1 [:- [b 1] [c 2] [f 3] [b 4]])  ;; duplicate b
       => '(3 3 2 2 4 1))

;; kw-rest tests

(define/kw/e (f2 [a 1] b . kw-rest) 
  kw-rest)
(check (f2 [:- [b 2]])
       => '((b . 2) (a . 1)))
(check (f2 [:- [b 2] [a 3]])
       => '((b . 2) (a . 3) (a . 1)))
(check-raised (let ()
                (define/kw/e (f2 [a 1] b . kw-rest) 
                  kw-rest)
                (f2)) 
  => missing-kw-arg?)
(check-raised (let ()
                (define/kw/e (f2 [a 1] b . kw-rest) 
                  kw-rest)
                (f2 [:- [c 'hmm]])) 
  => missing-kw-arg?)
;; unknown okay when there's a kw-rest
(check (f2 [:- [b 'foo] [c 'bar]]) 
       => '((b . foo) (c . bar) (a . 1)))

;; TODO: more extensive eval-time default argument values tests


;;;; TODO: Run-time keyword argument processing
;; TODO: define/kw/r tests


;;; TODO: using kw-proc as 1st-class

;;; TODO: attempting to set! a kw-proc binding





(check-report)
