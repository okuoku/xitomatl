#!r6rs
(import
  (rnrs)
  (rnrs eval)
  (xitomatl srfi lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl control))

(define-syntax check-SV
  (syntax-rules ()
    [(_ expr)
     (check (catch ex ([else (syntax-violation? ex)])
              (eval 'expr (environment '(rnrs) '(xitomatl control)))
              'unexpected-return)
            => #T)]))

(check (aif x (+ 1 2) 
         (* x x) 
         (assert #F))
       => 9)
(check (aif x (string? 'sym) 
         (assert #F) 
         (list x))
       => '(#F))
(check (aif x number? (+ 1 2)
         (* x x) 
         (assert #F))
       => 9)
(check (aif x integer? (+ 1.1 2)
         (assert #F) 
         (- x))
       => -3.1)
(let ([a 0] [b 0] [c 0] [d 0])
  (check (aif x (begin (set! a (+ 1 a)) integer?) (begin (set! b (+ 1 b)) (+ 1.1 2))
              (begin (set! c (+ 1 c)) 'bad) 
              (begin (set! d (+ 1 d)) (- x)))
         
         => -3.1)
  (check (list a b c d) => '(1 1 0 1)))
(check-SV (aif "oops" 'foo 'true 'false))

(let ([x 1] [n 0])
  (check (begin0 
           (begin (set! n (+ 1 n))
                  (- x))
           (set! x (+ 1 x))
           (set! x (* x x))) 
         => -1)
  (check x => 4)
  (check n => 1))


(check-report)
