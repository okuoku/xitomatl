;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

#!r6rs
(import 
  (rnrs)
  (rnrs eval)
  (xitomatl IU-match extras)
  (srfi :78 lightweight-testing))

(define-syntax must-be-a-syntax-error
  (syntax-rules ()
    [(_ expr)
     (check 
       (guard (ex [#t (syntax-violation? ex)])  
         (eval 'expr (environment '(rnrs) '(xitomatl IU-match extras))))
       => #t)]))


;; match-lambda tests

(check
  (map (match-lambda [,x (guard (symbol? x)) (symbol->string x)] [,x (expt 2 x)])
       '(asdf 42))
  => '("asdf" 4398046511104))

(check
  (map (match-lambda [(a ,b) (* b b)] [(,a b) (- a)])
       '((a 2) (3 b) (a 4)))
  => '(4 -3 16))

(check
  (map (match-lambda [(a ,(r) b) r] [#(,x) (+ x 123)])
       '((a #(321) b) #(1)))
  => '(444 124))

;; match-lambda* tests

(check
  (map (match-lambda* [(#(a ,x) . ,rest) (cons x (car rest))] [,x (reverse x)])
       '(#(a 321) 1)
       '(asdf 2)
       '(blah 3))
  => '((321 . asdf) (3 2 1)))

;; match-let tests

(check
  (match-let ([#(a (,x) ,y ,x)
               '#(a (#\λ) "ZZZ" #\λ)])
    (list y x))
  => '("ZZZ" #\λ))

(check
  (match-let ([#(a (,x) ,y ,x) 
               (vector 'a '(#\λ) "ZZZ" #\λ)]
              [(,a ,b ,c . #(,c ,c ,b ,a))
               '(1.1 2/3 3 . #(3 3 2/3 1.1))])
    (list x y a b c))
  => '(#\λ "ZZZ" 1.1 2/3 3))

(must-be-a-syntax-error
  (match-let ([,a 1] [,a 2])
    (- a)))

(must-be-a-syntax-error
  (match-let ([(,a (,b)) (list 1 (list 2))]
              [(,b ,a) (list 8 9)])
    (+ a b)))

(must-be-a-syntax-error
  (match-let ([#((#(,a) ,b . asdf) blah) (vector (list '#(1) 2 . 'asdf) 'blah)]
              [,x 'asdf]
              [,a (list 8 9)])
    (+ a b)))

(check
  (let ([a 'a] [b 'b])
    (match-let ([(,a ,b) (list 1 2)]
                [(,c ,d) (list a b)])
      (list a b c d)))
  => '(1 2 a b))

(must-be-a-syntax-error
  (match-let ([,(a) 1])
    (+ a a)))

(must-be-a-syntax-error
  (match-let ([,a 1]
              [#(,x ,(+ -> y) z) (vector 3 2 'z)])
    (+ x y a)))

;; match-let* tests

(check
  (match-let* ([#(a (,x) ,y ,x)
                '#(a (#\λ) "ZZZ" #\λ)])
    (list y x))
  => '("ZZZ" #\λ))

(check
  (match-let* ([#(a (,x) ,y ,x) 
                (vector 'a '(#\λ) "ZZZ" #\λ)]
               [(,a ,b ,c . #(,c ,c ,b ,a))
                '(1.1 2/3 3 . #(3 3 2/3 1.1))])
    (list x y a b c))
  => '(#\λ "ZZZ" 1.1 2/3 3))

(check
  (match-let* ([,a 1] [,a 2])
    (- a))
  => -2)

(check
  (match-let* ([(,a (,b)) (list 1 (list 2))]
               [(,b ,a) (list 8 9)])
    (+ a b))
  => 17)

(check
  (match-let* ([#((#(,a) ,b . asdf) blah) (vector (cons* '#(1) 2 'asdf) 'blah)]
               [,x 'asdf]
               [,a (list 8 9)])
    (vector a b))
  => '#((8 9) 2))

(check
  (let ([a 'a] [b 'b])
    (match-let* ([(,a ,b) (list 1 2)]
                 [(,c ,d) (list a b)])
      (list a b c d)))
  => '(1 2 1 2))

(check
  (let ([a 'a] [b 'b])
    (match-let* ([(,c ,d) (list a b)]
                 [(,a ,b) (list 1 2)])
      (list a b c d)))
  => '(1 2 a b))

(must-be-a-syntax-error
  (match-let* ([,(a) 1])
    (+ a a)))

(must-be-a-syntax-error
  (match-let* ([,a 1]
               [#(,x ,(+ -> y) z) (vector 3 2 'z)])
    (+ x y a)))

;; match-letrec tests

(check
  (match-letrec ([,f (lambda (x) (g x))]
                 [(,g #(,h)) (list (lambda (x) (h x)) 
                                   (vector (lambda (x) (* x x))))])
    (f 2))
  => 4)

(must-be-a-syntax-error
  (match-letrec ([,f (lambda (x) (g x))]
                 [#(,g ,h) (vector (lambda (x) (h x)) (lambda (x) (* x x)))]
                 [(((((,f))))) (list (list (list (list (list (lambda (x) 'blah))))))])
    (f 2)))

(must-be-a-syntax-error
  (match-letrec ([(asdf ,(f)) 'cant-auto-recur]
                 [(,g #(,h)) (list (lambda (x) (h x)) 
                                   (vector (lambda (x) (* x x))))])
    (f 2)))


;; match-define tests

(match-define (,x a #(b ,y) . ((,z))) 
  (cons* 1 'a (vector 'b 2) '((3))))
(check (list x y z) => '(1 2 3))



(check-report)
