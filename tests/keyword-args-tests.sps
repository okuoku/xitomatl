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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Expand-time keyword argument processing: define/kw and define/kw-r

;;;; define/kw does:
;;;; Evaluation of default value expressions once at the time of 
;;;; evaluation of the define/kw

(define/kw (fde0 a [b 123])
  (list a b))
(check (fde0 [:- [a 'foo]]) => '(foo 123))
(check (fde0 [:- [b "asdf"] [a 'bar]]) => '(bar "asdf"))
(check-raised (let ()
                (define/kw (fde0 a [b 123])
                  (list a b))
                (fde0)) 
  => missing-kw-arg?)
(check-raised (let ()
                (define/kw (fde0 a [b 123])
                  (list a b))
                (fde0 [:- [a 'foo] [c 'hmm]])) 
  => unknown-kw-arg?)

(define/kw (fde1 [a 1] b c [d 2] [e 3] f)
  (list f e d c b a))
(check (fde1 [:- [d 1] [f 2] [a 3] [e 4] [c 5] [b 6]]) 
       => '(2 4 1 5 6 3))
(check (fde1 [:- [b 1] [c 2] [f 3] [b 4]])  ;; duplicate b
       => '(3 3 2 2 4 1))
(check (fde1 [:- [b 1] [c 2] [f 3] [b 4] [b 5] [a -1] [f 42] [d -2]])  ;; duplicate b (twice) and f
       => '(42 3 -2 2 5 -1))

;; kw-rest tests

(define/kw (fde2 [a 1] b . kw-rest) 
  kw-rest)
(check (fde2 [:- [b 2]])
       => '((b . 2) (a . 1)))  ;; supplieds always preceed defaults
(check (cdr (assoc 'a (fde2 [:- [b 2] [a 3]])))
       => 3)
;; unknown okay when there's a kw-rest
(check (cdr (assoc 'c (fde2 [:- [b 'foo] [c 'bar]]))) 
       => 'bar)
(check (let ([r  ;; duplicate b (twice) and f
              (fde2 [:- [b 1] [c 2] [f 3] [b 4] [b 5] [a -1] [f 42] [d -2]])])
         (map (lambda (n) (cdr (assoc n r))) '(a b c d f)))  
       ;; last dup is the primary one
       => '(-1 5 2 -2 42))
(check-raised (let ()
                (define/kw (fde2 [a 1] b . kw-rest) 
                  kw-rest)
                (fde2)) 
  => missing-kw-arg?)
(check-raised (let ()
                (define/kw (fde2 [a 1] b . kw-rest) 
                  kw-rest)
                (fde2 [:- [c 'hmm]])) 
  => missing-kw-arg?)

;; eval-time default argument values tests

(define x0 "adsf") (define y0 (list 1))

(define/kw (fde3 [a x0] b [c y0])
  (list a b c))
(check (fde3 [:- [b 'zzz]]) 
       (=> (lambda (v0 v1) (for-all eq? v0 v1)))
       (list x0 'zzz y0))

(define/kw (fde4 [a (vector x0 y0)])
  (vector-ref a 1))
(check (fde4) (=> eq?) y0)

;;;; define/kw-r does:
;;;; Evaluation of default value expressions every time 
;;;; the body of the procedure is run.

(define/kw-r (fdr0 a [b 123])
  (list a b))
(check (fdr0 [:- [a 'foo]]) => '(foo 123))
(check (fdr0 [:- [b "asdf"] [a 'bar]]) => '(bar "asdf"))
(check-raised (let ()
                (define/kw-r (fdr0 a [b 123])
                  (list a b))
                (fdr0)) 
  => missing-kw-arg?)
(check-raised (let ()
                (define/kw-r (fdr0 a [b 123])
                  (list a b))
                (fdr0 [:- [a 'foo] [c 'hmm]])) 
  => unknown-kw-arg?)

(define/kw-r (fdr1 [a 1] b c [d 2] [e 3] f)
  (list f e d c b a))
(check (fdr1 [:- [d 1] [f 2] [a 3] [e 4] [c 5] [b 6]]) 
       => '(2 4 1 5 6 3))
(check (fdr1 [:- [b 1] [c 2] [f 3] [b 4]])  ;; duplicate b
       => '(3 3 2 2 4 1))
(check (fdr1 [:- [b 1] [c 2] [f 3] [b 4] [b 5] [a -1] [f 42] [d -2]])  ;; duplicate b (twice) and f
       => '(42 3 -2 2 5 -1))

;; kw-rest tests

(define/kw-r (fdr2 [a 1] b . kw-rest) 
  kw-rest)
(check (fdr2 [:- [b 2]])
       => '((b . 2) (a . 1)))
(check (cdr (assoc 'a (fdr2 [:- [b 2] [a 3]])))
       => 3)
;; unknown okay when there's a kw-rest
(check (cdr (assoc 'c (fdr2 [:- [b 'foo] [c 'bar]]))) 
       => 'bar)
(check (let ([r  ;; duplicate b (twice) and f
              (fdr2 [:- [b 1] [c 2] [f 3] [b 4] [b 5] [a -1] [f 42] [d -2]])])
         (map (lambda (n) (cdr (assoc n r))) '(a b c d f)))  
       ;; last dup is the primary one
       => '(-1 5 2 -2 42))
(check-raised (let ()
                (define/kw-r (fdr2 [a 1] b . kw-rest) 
                  kw-rest)
                (fdr2)) 
  => missing-kw-arg?)
(check-raised (let ()
                (define/kw-r (fdr2 [a 1] b . kw-rest) 
                  kw-rest)
                (fdr2 [:- [c 'hmm]])) 
  => missing-kw-arg?)

;; More extensive run-time default argument values tests

(define x1 "adsf") (define y1 (list 1))

(define/kw-r (fdr3 [a x1] b [c y1])
  (list a b c))
(check (let ([x1 'bork] [y1 "bork"])  ;; attempt to screw up by shadowing, hygiene should prevent
         (fdr3 [:- [b 'zzz]])) 
       (=> (lambda (v0 v1) (for-all eq? v0 v1)))
       (list x1 'zzz y1))
(set! x1 "qwerty") (set! y1 (vector 42))
(check (let ([x1 'bork] [y1 "bork"])  ;; attempt to screw up by shadowing, hygiene should prevent
         (fdr3 [:- [b 'zzz]])) 
       => '("qwerty" zzz #(42)))

(define/kw-r (fdr4 [a (vector x1 y1)])
  (vector-ref a 1))
(check (fdr4) (=> eq?) y1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Run-time keyword argument processing: lambda/kw and lambda/kw-r

;;;; lambda/kw does:
;;;; Evaluation of default value expressions once at the time of 
;;;; evaluation of the lambda/kw

(define fle0 
  (lambda/kw (a [b 123])
    (list a b)))
(check (fle0 [:- [a 'foo]]) => '(foo 123))
(check (fle0 [:- [b "asdf"] [a 'bar]]) => '(bar "asdf"))
(check-raised (let ()
                ((lambda/kw (a [b 123])
                   (list a b)))) 
  => missing-kw-arg?)
(check-raised (let ()
                ((lambda/kw (a [b 123])
                   (list a b))
                 [:- [a 'foo] [c 'hmm]])) 
  => unknown-kw-arg?)

(define fle1 
  (lambda/kw ([a 1] b c [d 2] [e 3] f)
    (list f e d c b a)))
(check (fle1 [:- [d 1] [f 2] [a 3] [e 4] [c 5] [b 6]]) 
       => '(2 4 1 5 6 3))
(check (fle1 [:- [b 1] [c 2] [f 3] [b 4]])  ;; duplicate b
       => '(3 3 2 2 4 1))
(check (fle1 [:- [b 1] [c 2] [f 3] [b 4] [b 5] [a -1] [f 42] [d -2]])  ;; duplicate b (twice) and f
       => '(42 3 -2 2 5 -1))

;; kw-rest tests

(define fle2 
  (lambda/kw ([a 1] b . kw-rest) 
    kw-rest))
(check (fle2 [:- [b 2]])
       => '((b . 2) (a . 1)))  ;; supplieds always preceed defaults
(check (cdr (assoc 'a (fle2 [:- [b 2] [a 3]])))
       => 3)
;; unknown okay when there's a kw-rest
(check (cdr (assoc 'c (fle2 [:- [b 'foo] [c 'bar]]))) 
       => 'bar)
(check (let ([r  ;; duplicate b (twice) and f
              (fle2 [:- [b 1] [c 2] [f 3] [b 4] [b 5] [a -1] [f 42] [d -2]])])
         (map (lambda (n) (cdr (assoc n r))) '(a b c d f)))  
       ;; last dup is the primary one
       => '(-1 5 2 -2 42))
(check-raised (let ()
                ((lambda/kw ([a 1] b . kw-rest) 
                   kw-rest))) 
  => missing-kw-arg?)
(check-raised (let ()
                ((lambda/kw ([a 1] b . kw-rest) 
                   kw-rest)
                 [:- [c 'hmm]])) 
  => missing-kw-arg?)

;; eval-time default argument values tests

(define lx0 "adsf") (define ly0 (list 1))

(define fle3 
  (lambda/kw ([a lx0] b [c ly0])
    (list a b c)))
(check (fle3 [:- [b 'zzz]]) 
       (=> (lambda (v0 v1) (for-all eq? v0 v1)))
       (list lx0 'zzz ly0))

(check ((lambda/kw ([a (vector lx0 ly0)])
          (vector-ref a 1)))
       (=> eq?) ly0)

;;;; lambda/kw-r does:
;;;; Evaluation of default value expressions every time 
;;;; the body of the procedure is run.

(define flr0 
  (lambda/kw-r (a [b 123])
    (list a b)))
(check (flr0 [:- [a 'foo]]) => '(foo 123))
(check (flr0 [:- [b "asdf"] [a 'bar]]) => '(bar "asdf"))
(check-raised (let ()
                ((lambda/kw-r (a [b 123])
                  (list a b)))) 
  => missing-kw-arg?)
(check-raised (let ()                
                ((lambda/kw-r (a [b 123])
                   (list a b)) 
                 [:- [a 'foo] [c 'hmm]])) 
  => unknown-kw-arg?)

(define flr1 
  (lambda/kw-r ([a 1] b c [d 2] [e 3] f)
    (list f e d c b a)))
(check (flr1 [:- [d 1] [f 2] [a 3] [e 4] [c 5] [b 6]]) 
       => '(2 4 1 5 6 3))
(check (flr1 [:- [b 1] [c 2] [f 3] [b 4]])  ;; duplicate b
       => '(3 3 2 2 4 1))
(check (flr1 [:- [b 1] [c 2] [f 3] [b 4] [b 5] [a -1] [f 42] [d -2]])  ;; duplicate b (twice) and f
       => '(42 3 -2 2 5 -1))

;; kw-rest tests

(define flr2 
  (lambda/kw-r ([a 1] b . kw-rest) 
    kw-rest))
(check (flr2 [:- [b 2]])
       => '((b . 2) (a . 1)))
(check (cdr (assoc 'a (flr2 [:- [b 2] [a 3]])))
       => 3)
;; unknown okay when there's a kw-rest
(check (cdr (assoc 'c (flr2 [:- [b 'foo] [c 'bar]]))) 
       => 'bar)
(check (let ([r  ;; duplicate b (twice) and f
              (flr2 [:- [b 1] [c 2] [f 3] [b 4] [b 5] [a -1] [f 42] [d -2]])])
         (map (lambda (n) (cdr (assoc n r))) '(a b c d f)))  
       ;; last dup is the primary one
       => '(-1 5 2 -2 42))
(check-raised (let ()
                ((lambda/kw-r (flr2 [a 1] b . kw-rest) 
                  kw-rest))) 
  => missing-kw-arg?)
(check-raised (let ()                
                ((lambda/kw-r (flr2 [a 1] b . kw-rest) 
                   kw-rest)
                 [:- [c 'hmm]])) 
  => missing-kw-arg?)

;; More extensive run-time default argument values tests

(define x2 "adsf") (define y2 (list 1))

(define flr3 
  (lambda/kw-r ([a x2] b [c y2])
    (list a b c)))
(check (let ([x2 'bork] [y2 "bork"])  ;; attempt to screw up by shadowing, hygiene should prevent
         (flr3 [:- [b 'zzz]])) 
       (=> (lambda (v0 v1) (for-all eq? v0 v1)))
       (list x2 'zzz y2))
(set! x2 "qwerty") (set! y2 (vector 42))
(check (let ([x2 'bork] [y2 "bork"])  ;; attempt to screw up by shadowing, hygiene should prevent
         (flr3 [:- [b 'zzz]])) 
       => '("qwerty" zzz #(42)))

(check ((lambda/kw-r ([a (vector x2 y2)])
          (vector-ref a 1)))
       (=> eq?) y2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Using a define/kw or define/kw-r as 1st-class

(define/kw (fc0 a [b 1])
  (list a b))
(check (procedure? fc0) => #t)
(define fc0* fc0)
(check (fc0* [:- [a 2]]) => '(2 1))

(define xfc1 1)
(define/kw-r (fc1 [a xfc1] b)
  (list a b))
(check (procedure? fc1) => #t)
(define fc1* fc1)
(check (fc1* [:- [b 2]]) => '(1 2))
(set! xfc1 "asdf")
(check (fc1* [:- [b 2]]) => '("asdf" 2))

(define/kw (fc2 a [b 1] . kw-rest)
  kw-rest)
(check (procedure? fc2) => #t)
(define fc2* fc2)
(check (cdr (assoc 'c (fc2* [:- [c 3] [a 2]]))) => 3)
(check (cdr (assoc 'b (fc2* [:- [b 5] [c 3] [a 2] [b 4]]))) => 4)

(define xfc3 1)
(define/kw-r (fc3 [a xfc3] b . kw-rest)
  kw-rest)
(check (procedure? fc3) => #t)
(define fc3* fc3)
(check (cdr (assoc 'a (fc3* [:- [b 2]]))) => 1)
(check (cdr (assoc 'c (fc3* [:- [b 2] [c 3]]))) => 3)
(check (cdr (assoc 'b (fc3* [:- [b 5] [c 3] [a 2] [b 4]]))) => 4)
(set! xfc3 "asdf")
(check (cdr (assoc 'a (fc3* [:- [b 2] [z 4]]))) => "asdf")

;;; Attempting to set! a define/kw or define/kw-r binding

(check-raised (let ()
                (define/kw (ds0 a) a)
                (set! ds0 'foo)) 
  => syntax-violation?)
(check-raised (let ()
                (define/kw-r (ds1 a) a)
                (set! ds1 'foo)) 
  => syntax-violation?)



(check-report)
