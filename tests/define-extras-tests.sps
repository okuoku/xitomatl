#!r6rs
(import
  (rename (rnrs) (define rnrs:define) (define-syntax rnrs:define-syntax))
  (rnrs eval)
  (xitomatl define extras)
  (xitomatl conditions)
  (xitomatl srfi lightweight-testing))
  
(rnrs:define-syntax check-syntax-error
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [(syntax-violation? ex) #t]
                       [else `(dont-know: ,ex)])
              (eval '(let () expr) 
                    (environment '(except (rnrs) define define-syntax) 
                                 '(xitomatl define extras))) 
              '(succeeded: expr))
            => #t)]))

(rnrs:define-syntax check-assertion-error
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [(assertion-violation? ex) #t]
                       [else `(dont-know: ,ex)])
              (let () expr '(succeeded: expr)))
            => #t)]))

(rnrs:define-syntax check-assertion-error/msg
  (lambda (stx)
    (syntax-case stx ()
      [(_ msg expr)
       (string? (syntax->datum #'msg))
       #'(check (guard (ex [(and (assertion-violation? ex)
                                 (message-condition? ex))
                            (condition-message ex)]
                           [else `(dont-know: ,ex)])
                  (let () expr '(succeeded: expr)))
                => msg)])))

(rnrs:define-syntax check-assertion-error/who
  (lambda (stx)
    (syntax-case stx ()
      [(_ who expr)
       (symbol? (syntax->datum #'who))
       #'(check (guard (ex [(and (assertion-violation? ex)
                                 (who-condition? ex))
                            (condition-who ex)]
                           [else `(dont-know: ,ex)])
                  (let () expr '(succeeded: expr)))
                => 'who)])))

(rnrs:define-syntax check-assertion-error/msg/AN
  (lambda (stx)
    (syntax-case stx ()
      [(_ msg an expr)
       (and (string? (syntax->datum #'msg))
            (identifier? #'an))
       #'(check (guard (ex [(and (assertion-violation? ex)
                                 (message-condition? ex)
                                 (argument-name-condition? ex))
                            (list (condition-message ex)
                                  (condition-argument-name ex))]
                           [else `(dont-know: ,ex)])
                  (let () expr '(succeeded: expr)))
                => '(msg an))])))

(rnrs:define-syntax check-no-error
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [#t #f])
              (let () expr #t))
            => #t)]))

;;; define-values

(define-values (a) 1)
(check a => 1)
(check-no-error (define-values () (values)))
(define-values (b c) (values 2 3))
(check (list b c) => '(2 3))
(define-values (d e f g h i j k l m n o p q r s t u v w x y z) 
  (values 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
(check (list d e f g h i j k l m n o p q r s t u v w x y z)
       => '(4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
(define-values all0 (values))
(check all0 => '())
(define-values all1 (values 1))
(check all1 => '(1))
(define-values all2 (values 1 2 3 4))
(check all2 => '(1 2 3 4))
(define-values (aa . r0) (values 1 2 3 4))
(check aa => 1)
(check r0 => '(2 3 4))
(define-values (bb cc . r1) (values 1 2))
(check bb => 1)
(check cc => 2)
(check r1 => '())
(check-assertion-error
  (define-values (a) (values 1 2)))
(check-assertion-error
  (define-values (a b c) (values 1 2)))
(check-assertion-error
  (define-values (a b c d e f g) (values 1 2 3)))
(check-assertion-error
  (define-values () (values 1 2)))
(check-assertion-error
  (define-values () (values 1)))
(check-syntax-error
 (define-values (a b a) (values 1 2 3)))

;;; currying

(define (fn x) x)
(check (fn 678) => 678)
(define ((fc a) b) (- b a))
(check ((fc 5) 9) => 4)
(define ((((fc2 a) b) c) d) (+ a b c d))
(check ((((fc2 1) 2) 3) 4) => 10)

;;; defining macro transformers

(define-syntax M
  (lambda (stx)
    (syntax-case stx () [(_ x) #'(reverse x)])))
(check (M '(a b c)) => '(c b a))
(define-syntax (M2 stx)
  (syntax-case stx () [(_ x) #'(list x)]))
(check (M2 'zzz) => '(zzz))

;;; case-lambda/AV and friends

(check ((case-lambda/AV [() (AV "oops")] [r r]) 1 2 3)
       => '(1 2 3))
(check-assertion-error/msg "oops1" 
 ((case-lambda/AV [() (AV "oops1" 'ign)] [ign #f])))
(check ((λ/AV (a) (list->string (reverse (string->list a)))) "asdf")
       => "fdsa")
(check-assertion-error/msg "oops2"
 ((λ/AV () (AV "oops2"))))
(let ()
  (define/AV (f x y . zs) 
    (when (null? zs) (AV "zs null" zs))
    (apply * x y zs))
  (check (f 1 2 3 4 5) => 120))
(check-assertion-error/msg "oops3"
 (let ()
   (define/AV (f) (AV "oops3" 'ign 'ign 'ign))
   (f)))
(check-assertion-error/who f
  (let ()
    (define/AV f (lambda () (AV "oops")))
    (f)))

;;; case-lambda/? and friends

(check ((case-lambda/? [() 'a] [(x) 'b]))
       => 'a)
(check ((case-lambda/? [() 'a] [(x) 'b]) 1)
       => 'b)
(check ((case-lambda/? [() 'a] [([x integer?]) 'b]) 1)
       => 'b)
(check ((case-lambda/? [() 'a] [r 'b]) 1) => 'b)
(check ((case-lambda/? [rest (reverse rest)] [([x integer?]) 'b]) 1 2 3)
       => '(3 2 1))
(check ((case-lambda/? [#(rest list?) (reverse rest)] [([x integer?]) 'b]) 1 2 3)
       => '(3 2 1))
(check ((case-lambda/? [(x [y string?] z . #(r (lambda (r) (for-all number? r)))) 
                        (cons* x y z r)] 
                       [(x) 'b])
        'x "yy" #\z 1 -56.3 67/902)
       => '(x "yy" #\z 1 -56.3 67/902))
(check ((case-lambda/? [([x symbol?] [y string?] [z char?] . #(r (lambda (r) (for-all number? r)))) 
                        (cons* x y z r)] 
                       [(x) 'b])
        'x "yy" #\z 1 -56.3 67/902)
       => '(x "yy" #\z 1 -56.3 67/902))
(check-syntax-error
 ((case-lambda/? [() 'a] [([x]) 'b]) 1))
(check-syntax-error
 ((case-lambda/? [() 'a] [([x integer? oops]) 'b]) 1))
(check-syntax-error
 ((case-lambda/? [#() (reverse rest)] [([x integer?]) 'b]) 1 2 3))
(check-syntax-error
 ((case-lambda/? [#([rest oops]) (reverse rest)] [([x integer?]) 'b]) 1 2 3))
(check-syntax-error
 ((case-lambda/? [#(rest list? oops) (reverse rest)] [([x integer?]) 'b]) 1 2 3))
(check-syntax-error
 ((case-lambda/? [(x [y string?] z . #([r (lambda (r) (for-all number? r))])) 
                  (cons* x y z r)] 
                 [(x) 'b])
  'x "yy" #\z 1 -56.3 67/902))
(check-assertion-error/msg/AN "argument check failed" x
 ((case-lambda/? [() 'a] [([x string?]) 'b]) 1))
(check-assertion-error/msg/AN "argument check failed" y
 ((case-lambda/? [() 'a] [([x string?] [y char?]) (values x y)]) "" 'oops))
(check-assertion-error/msg/AN "argument check failed" rest
 ((case-lambda/? [#(rest char?) (reverse rest)] [() 'b]) 1 2 3))
(check-assertion-error/msg/AN "argument check failed" r
 ((case-lambda/? [(x [y string?] z . #(r (lambda (r) (for-all negative? r)))) 
                  (cons* x y z r)] 
                 [(x) 'b])
  'x "yy" #\z 1 -56.3 67/902))

(check ((λ/? () 'a))
       => 'a)
(check ((λ/? (x) 'b) 1)
       => 'b)
(check ((λ/? ([x integer?]) 'b) 1)
       => 'b)
(check ((λ/? rest (reverse rest)) 1 2 3)
       => '(3 2 1))
(check ((λ/? #(rest list?) (reverse rest)) 1 2 3)
       => '(3 2 1))
(check ((λ/? (x [y string?] z . #(r (lambda (r) (for-all number? r)))) 
          (cons* x y z r))
        'x "yy" #\z 1 -56.3 67/902)
       => '(x "yy" #\z 1 -56.3 67/902))
(check ((λ/? ([x symbol?] [y string?] [z char?] . #(r (lambda (r) (for-all number? r)))) 
          (cons* x y z r))
        'x "yy" #\z 1 -56.3 67/902)
       => '(x "yy" #\z 1 -56.3 67/902))
(check-syntax-error
 ((λ/? ([x]) 'b) 1))
(check-syntax-error
 ((λ/? ([x integer? oops]) 'b) 1))
(check-syntax-error
 ((λ/? #() 'a)))
(check-syntax-error
 ((λ/? #([rest oops]) (reverse rest)) 1 2 3))
(check-syntax-error
 ((λ/? #(rest list? oops) (reverse rest)) 1 2 3))
(check-syntax-error
 ((λ/? (x [y string?] z . #([r (lambda (r) (for-all number? r))])) 
    (cons* x y z r))
  'x "yy" #\z 1 -56.3 67/902))
(check-assertion-error/msg/AN "argument check failed" x
 ((λ/? ([x string?]) 'b) 1))
(check-assertion-error/msg/AN "argument check failed" y
 ((λ/? ([x string?] [y char?]) (values x y)) "" 'oops))
(check-assertion-error/msg/AN "argument check failed" rest
 ((λ/? #(rest char?) (reverse rest)) 1 2 3))
(check-assertion-error/msg/AN "argument check failed" r
 ((λ/? (x [y string?] z . #(r (lambda (r) (for-all negative? r)))) 
    (cons* x y z r))
  'x "yy" #\z 1 -56.3 67/902))

(check (let ()
         (define/? (f) 'a)
         (f))
       => 'a)
(check (let ()
         (define/? (f x) 'b) 
         (f 1))
       => 'b)
(check (let ()
         (define/? (f [x integer?]) 'b)
         (f 1))
       => 'b)
(check (let ()
         (define/? (f . rest) (reverse rest))
         (f 1 2 3))
       => '(3 2 1))
(check (let ()
         (define/? (f . #(rest list?)) (reverse rest))
         (f 1 2 3))
       => '(3 2 1))
(check (let ()
         (define/? (f x [y string?] z . #(r (lambda (r) (for-all number? r)))) 
           (cons* x y z r))
         (f 'x "yy" #\z 1 -56.3 67/902))
       => '(x "yy" #\z 1 -56.3 67/902))
(check (let ()
         (define/? (f [x symbol?] [y string?] [z char?] . #(r (lambda (r) (for-all number? r)))) 
           (cons* x y z r))
         (f 'x "yy" #\z 1 -56.3 67/902))
       => '(x "yy" #\z 1 -56.3 67/902))
(check-assertion-error/who foo
  (let ()
    (define/? foo (lambda/? ([x char?]) x))
    (foo 1)))
(check-assertion-error/who bar
  (let ()
    (define/? bar (case-lambda/? [([x char?]) x]))
    (bar 1)))
(check-syntax-error
 (let ()
   (define/? (f [x]) 'b) 
   (f 1)))
(check-syntax-error
 (let ()
   (define/? (f [x integer? oops]) 'b)
   (f 1)))
(check-syntax-error
 (let ()
   (define/? (f #()) 'a)
   (f)))
(check-syntax-error
 (let ()
   (define/? (f . #([rest oops])) (reverse rest))
   (f 1 2 3)))
(check-syntax-error
 (let ()
   (define/? (f . #(rest list? oops)) (reverse rest))
   (f 1 2 3)))
(check-syntax-error
 (let ()
   (define/? (f x [y string?] z . #([r (lambda (r) (for-all number? r))])) 
     (cons* x y z r))
   (f 'x "yy" #\z 1 -56.3 67/902)))
(check-assertion-error/msg/AN "argument check failed" x
 (let ()
   (define/? (f [x string?]) 'b)
   (f 1)))
(check-assertion-error/msg/AN "argument check failed" y
 (let ()
   (define/? (f [x string?] [y char?]) (values x y))
   (f "" 'oops)))
(check-assertion-error/msg/AN "argument check failed" rest
 (let ()
   (define/? (f . #(rest char?)) (reverse rest))
   (f 1 2 3)))
(check-assertion-error/msg/AN "argument check failed" r
 (let ()
   (define/? (f x [y string?] z . #(r (lambda (r) (for-all negative? r)))) 
     (cons* x y z r))
   (f 'x "yy" #\z 1 -56.3 67/902)))

;;; case-lambda/?/AV and friends

(check ((case-lambda/?/AV [() 'a] [(x) 'b]))
       => 'a)
(check ((case-lambda/?/AV [([x symbol?] [y string?] [z char?] . #(r (lambda (r) (for-all number? r)))) 
                        (cons* x y z r)] 
                       [(x) 'b])
        'x "yy" #\z 1 -56.3 67/902)
       => '(x "yy" #\z 1 -56.3 67/902))
(check ((case-lambda/?/AV [() 'a] [r 'b]) 1) => 'b)
(check-syntax-error
 ((case-lambda/?/AV [(x [y string?] z . #([r (lambda (r) (for-all number? r))])) 
                  (cons* x y z r)] 
                 [(x) 'b])
  'x "yy" #\z 1 -56.3 67/902))
(check-assertion-error/msg/AN "argument check failed" r
 ((case-lambda/?/AV [(x [y string?] z . #(r (lambda (r) (for-all negative? r)))) 
                  (cons* x y z r)] 
                 [(x) 'b])
  'x "yy" #\z 1 -56.3 67/902))
(check ((case-lambda/?/AV [() (AV "oops")] [#(r (lambda (x) (for-all integer? x))) r]) 1 2 3)
       => '(1 2 3))
(check-assertion-error/msg "oops1" 
 ((case-lambda/?/AV [([s symbol?]) (AV "oops1" 'ign)] [ign #f]) 'blah))

(check ((λ/?/AV () 'a))
       => 'a)
(check ((λ/?/AV ([x symbol?] [y string?] [z char?] . #(r (lambda (r) (for-all number? r)))) 
          (cons* x y z r))
        'x "yy" #\z 1 -56.3 67/902)
       => '(x "yy" #\z 1 -56.3 67/902))
(check ((λ/?/AV r 'b) 1) => 'b)
(check-syntax-error
 ((λ/?/AV (x [y string?] z . #([r (lambda (r) (for-all number? r))])) 
    (cons* x y z r))
  'x "yy" #\z 1 -56.3 67/902))
(check-assertion-error/msg/AN "argument check failed" r
 ((λ/?/AV (x [y string?] z . #(r (lambda (r) (for-all negative? r)))) 
    (cons* x y z r))
  'x "yy" #\z 1 -56.3 67/902))
(check ((λ/?/AV ([a string?]) (list->string (reverse (string->list a)))) "asdf")
       => "fdsa")
(check-assertion-error/msg "oops2"
 ((λ/?/AV #(r null?) (AV "oops2"))))

(check (let ()
         (define/?/AV (f) 'a)
         (f))
       => 'a)
(check (let ()
         (define/?/AV (f [x symbol?] [y string?] [z char?] . #(r (lambda (r) (for-all number? r)))) 
           (cons* x y z r))
         (f 'x "yy" #\z 1 -56.3 67/902))
       => '(x "yy" #\z 1 -56.3 67/902))
(check
 (let ()
   (define/?/AV (f . r) 'b)
   (f 1))
 => 'b)
(let ()
  (define/?/AV asdf (lambda/? ([x char?]) (AV "oops")))
  (check-assertion-error/who asdf
    (asdf 1))
  (check-assertion-error/who asdf
    (asdf #\c)))
(let ()
  (define/?/AV asdf (case-lambda/? [([x char?]) x] [() (AV "oops")]))
  (check-assertion-error/who asdf
    (asdf 1))
  (check-assertion-error/who asdf
    (asdf)))
(check-syntax-error
 (let ()
   (define/?/AV (f x [y string?] z . #([r (lambda (r) (for-all number? r))])) 
     (cons* x y z r))
   (f 'x "yy" #\z 1 -56.3 67/902)))
(check-assertion-error/msg/AN "argument check failed" r
 (let ()
   (define/?/AV (f x [y string?] z . #(r (lambda (r) (for-all negative? r)))) 
     (cons* x y z r))
   (f 'x "yy" #\z 1 -56.3 67/902)))
(let ()
  (define/?/AV (f x [y integer?] . zs) 
    (when (null? zs) (AV "zs null" zs))
    (apply * x y zs))
  (check (f 1 2 3 4 5) => 120))
(check-assertion-error/msg "oops3"
 (let ()
   (define/?/AV (f) (AV "oops3" 'ign 'ign 'ign))
   (f)))


(check-report)
