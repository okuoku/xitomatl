#!/usr/bin/env scheme-script
#!r6rs
(import
  (rnrs)
  (xitomatl lists)
  (xitomatl srfi lightweight-testing))

(define-syntax check-AV
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (assertion-violation? ex)])
              expr
              'unexpected-return)
            => #t)]))

(define L (do ([i #e1e5 (- i 1)]
               [l '() (cons i l)]) 
            [(< i 0) l]))

;;; make-list
(check (length (make-list 123456)) => 123456)
(check (make-list 4 'x) => '(x x x x))
(check-AV (make-list -1))
(check-AV (make-list 'oops))
;;; last-pair
(check (last-pair '(1 . 2)) => '(1 . 2))
(check (last-pair '(1 2 3 4 5 6 7 8 9 . 10)) => '(9 . 10))
(check (last-pair '(1 2 3 4 5 6 7 8)) => '(8))
(check-AV (last-pair '#(oops)))
;;; map/left-right/preserving
(let ([a '()] [l L])
  (map/left-right/preserving (lambda (i) (set! a (cons i a))) l)
  (check (reverse a) => l))
(let ([x (string #\a)] [y (vector 'b)] [z (list 1)])
  (let ([l (list x y z)])
    (check (map/left-right/preserving values l) (=> eq?) l)))
;;; map/filter
(check (map/filter odd? L) => (filter values (map odd? L)))
(check (map/filter (lambda (x y z) (odd? (+ x y z))) L L L)
       => (filter values (map (lambda (x y z) (odd? (+ x y z))) L L L)))
;;; remove-dups
(check (remove-dups '(a "a" (b 2) c (b 2) (b 2) a d "a")) 
       => '(a "a" (b 2) c d))
;;; remv-dups
(check (remv-dups (list (list 1) 2 (list 1) 2 2 3 (list 1) #\c #\a #\c)) 
       => '((1) 2 (1) 3 (1) #\c #\a))
;;; remq-dups
(let ([x (string #\s)] [y (vector 'v)] [z (list 1)])
  (check (remq-dups (list "s" y x '(1) x z x z (vector 'v) y (string #\s)))
         => '("s" #(v) "s" (1) (1) #(v) "s")))
;;; intersperse
(check (intersperse '() 1) => '())
(check (intersperse '(a) 1) => '(a))
(check (intersperse '(a b) 1) => '(a 1 b))
(check (intersperse '(a b c) 1) => '(a 1 b 1 c))
(check (intersperse '(a b c d e f g h i j k l m n o p q r s t u v w x y z) 1) 
       => '(a 1 b 1 c 1 d 1 e 1 f 1 g 1 h 1 i 1 j 1 k 1 l 1 m 1 n 1 o 1 p 1 q 1 r 1 s 1 t 1 u 1 v 1 w 1 x 1 y 1 z))


(check-report)
