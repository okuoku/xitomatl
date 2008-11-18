#!r6rs
(import 
  (rnrs)
  (only (xitomatl common) add1 time)
  (xitomatl keywords))

;;; TODO: consult with people about better/other ways to measure

(define-syntax bigloop
  (syntax-rules ()
    [(_ n body ...)
     (let loop ([i 0])
       (unless (= i n)
         body ...
         (loop (add1 i))))]))

(define N #e1e8)
(display "\nN = ") (display N) (newline)

(define (n1 a) 1)
(define (n2 a b) 1)
(define (n3 a b c) 1)

(define lekw1 (lambda/kw ([a]) 1))
(display "\nOne argument, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n1 1)))
(time (bigloop N (lekw1 'a 1)))

(define lekw2 (lambda/kw ([a] [b]) 1))
(display "\nTwo arguments, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n2 1 2)))
(time (bigloop N (lekw2 'b 2 'a 1)))

(define lekw3 (lambda/kw ([a] [b] [c]) 1))
(display "\nThree arguments, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n3 1 2 3)))
(time (bigloop N (lekw3 'b 2 'c 3 'a 1)))
