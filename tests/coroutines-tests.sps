#!r6rs
(import
  (rnrs)
  (xitomatl coroutines)
  (xitomatl srfi lightweight-testing)
  (xitomatl lists))

(define-syntax check-values
  (syntax-rules (=>)
    [(_ expr => (v ...))
     (check (let-values ([r expr]) r) => (list v ...))]))

(define-syntax check-finished
  (syntax-rules ()
    [(_ expr) 
     (check (guard (ex [else (coroutine-finished-condition? ex)])
              expr
              'unexpected-return)
            => #t)]))

(define-syntax check-AV
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (assertion-violation? ex)])
              expr
              'unexpected-return)
            => #t)]))

(define-syntax check-AV-msg-irrt
  (syntax-rules ()
    [(_ msg irrt expr)
     (check (guard (ex [else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (string=? msg (condition-message ex))
                                  (irritants-condition? ex)
                                  (= 1 (length (condition-irritants ex)))
                                  (eqv? irrt (car (condition-irritants ex))))])
              expr
              'unexpected-return)
            => #t)]))

;; procedural interface
(define g0
  (make-coroutine (lambda (Y)
                    (lambda (a b)
                      (Y a)
                      (Y b)))))
(check-AV (g0 'wrong 'number 'of 'args))
;; Failed first call does not start coroutine.
(check (g0 1 2) => 1)
(check (g0 'doesnt-matter) => 2)
(check-finished (g0))
(check-finished (g0))
(check-finished (g0))
(check-finished (g0))
(check-finished (g0))
;; syntactic interface and variable number of yielded values
(define-coroutine (g1 n)
  (do ([i 0 (+ 1 i)])
    [(= i n)]
    (apply yield (make-list i 'x))))
(check-values (g1 4) => ())
(check-values (g1 'doesnt-matter 'doesnt-matter 'doesnt-matter) => ('x))
(check-values (g1) => ('x 'x))
(check-values (g1 'doesnt-matter) => ('x 'x 'x))
(check-finished (g1))
(check-finished (g1))
;; lambda -like syntax
(define g2
  (coroutine args (for-each yield args)))
(check (g2 1 2 3) => 1)
(check (g2) => 2)
(check (g2) => 3)
(check-finished (g2))
;; case-lambda -like syntax and immediate finish
(define g3
  (case-coroutine
    [() 'no-yield]
    [args (yield 'failed)]))
(check-finished (g3))
;; illegal recursion detected and prevented
(define g4
  (case-coroutine
    [() (g4 'recursion 'not 'allowed)]
    [args (yield 'failed)]))
(check-AV-msg-irrt "illegal recursive or concurrent call" g4
  (g4))
;; resuming with supplied value(s) to use as co-routine
(define-coroutine (g5 . args)
  (yield (map yield args)))
(check (integer? (g5 1 2 3 4)) => #t)
(check (integer? (g5 'a)) => #t)
(check (integer? (g5 'b)) => #t)
(check (integer? (g5 'c)) => #t)
(check (let ([r (g5 'd)]) 
         (and (list r)
              (= 4 (length r))
              (for-all symbol? r))) 
       => #t)
(check-finished (g5))
(define-coroutine (g6)
  (let loop ([x 'initial])
    (let-values ([in (yield x)])
      (for-each loop in))))
(check (g6) => 'initial)
(check (g6 1 2) => 1)
(check (g6) => 2)
(check (g6 3) => 3)
(check-finished (g6))
;; exceptions are raised in the dynamic environment of the particular call to a coroutine
(define-coroutine (g7)
  (yield 1) 
  (yield 2))
(check (guard (ex [(coroutine-finished-condition? ex) 'first-dyn-env])
         (g7))
       => 1)
(check (guard (ex [(coroutine-finished-condition? ex) 'second-dyn-env])
         (g7))
       => 2)
(check (guard (ex [(coroutine-finished-condition? ex) 'third-dyn-env])
         (g7))
       => 'third-dyn-env)
(define-coroutine (g8)
  (yield 1)
  (yield 2)
  (raise 'prob))
(check (guard (ex [(eq? ex 'prob) 'first-dyn-env])
         (g8))
       => 1)
(check (guard (ex [(eq? ex 'prob) 'second-dyn-env])
         (g8))
       => 2)
(check (guard (ex [(eq? ex 'prob) 'third-dyn-env])
         (g8))
       => 'third-dyn-env)
(check (guard (ex [(eq? ex 'prob) 'fourth-dyn-env])
         (g8))
       => 'fourth-dyn-env)
;; coroutine not re-entered after finished
(define return-count 0)
(define-coroutine (g9)
  (yield 1)
  (set! return-count (+ 1 return-count)))
(check (g9) => 1)
(check-finished (g9))
(check return-count => 1)
(check-finished (g9))
(check return-count => 1)
(check-finished (g9))
(check return-count => 1)

(check-report)
