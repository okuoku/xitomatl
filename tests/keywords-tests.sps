;;; Copyright (c) 2008 Derick Eddington
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; Except as contained in this notice, the name(s) of the above copyright
;;; holders shall not be used in advertising or otherwise to promote the sale,
;;; use or other dealings in this Software without prior written authorization.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!r6rs
(import
  (rnrs)
  (rnrs eval)
  (xitomatl keywords)
  (xitomatl srfi lightweight-testing)
  (xitomatl conditions))

(define-syntax check-values
  (syntax-rules ()
    [(_ expr => v ...)
     (check (let-values ([vals expr]) vals) => (list v ...))]))

(define-syntax check-missing-keyword
  (syntax-rules ()
    [(_ kw-name expr)
     (check (guard (ex [else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (keyword-condition? ex)
                                  (list (condition-message ex)
                                        (condition-keyword ex)))])
              expr
              'unexpected-return)
            => '("missing required keyword" kw-name))]))

(define-syntax check-missing-value
  (syntax-rules ()
    [(_ kw-name expr)
     (check (guard (ex [else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (keyword-condition? ex)
                                  (list (condition-message ex)
                                        (condition-keyword ex)))])
              expr
              'unexpected-return)
            => '("keyword missing value" kw-name))]))

(define-syntax check-pred-failed
  (syntax-rules ()
    [(_ kw-name pred-form expr)
     (check (guard (ex [else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (keyword-condition? ex)
                                  (predicate-condition? ex)
                                  (list (condition-message ex)
                                        (condition-keyword ex)
                                        (condition-pred ex)))])
              expr
              'unexpected-return)
            => '("keyword predicate false" kw-name pred-form))]))

(define-syntax check-invalid-options
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (and (syntax-violation? ex)
                                  (message-condition? ex)
                                  (condition-message ex))])
              (eval 'expr
                    (environment '(rnrs) '(xitomatl keywords)))
              'unexpected-return)
            => "invalid options for keyword")]))

(define-syntax check-invalid-formals
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (and (syntax-violation? ex)
                                  (message-condition? ex)
                                  (condition-message ex))])
              (eval 'expr
                    (environment '(rnrs) '(xitomatl keywords)))
              'unexpected-return)
            => "invalid keyword formals")]))

(define-syntax check-no-clause
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (condition-message ex))])
              expr
              'unexpected-return)
            => "no clause matches arguments")]))

;; most basic

(define parser0 (keywords-parser))
(check (parser0 '()) => '())
(check (parser0 '(1 2 3)) => '(1 2 3))

(define parser1 (keywords-parser [a]))
(check-missing-keyword a (parser1 '()))
(check-missing-keyword a (parser1 '(b 1)))
(check-missing-value a (parser1 '(a)))
(check-values (parser1 '(a 1)) => 1 '())
(check-values (parser1 '(a a)) => 'a '())
(check-values (parser1 '(a 1 a 2)) => 2 '())
(check-values (parser1 '(a a a 2)) => 2 '())
(check-values (parser1 '(a 1 a a)) => 'a '())
(check-values (parser1 '(a 1 b 2)) => 1 '(b 2))
(check-values (parser1 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a 8)) 
              => 8 '(b 2 "foo" b 4 #\b #\a #\r c 6 b 7))
(check-missing-value a (parser1 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a)))

;; invalid options for keyword specification 
;; (These 19 aren't working on PLT because it mutilates the raised conditions.)

(check-invalid-options (keywords-parser [a oops]))
(check-invalid-options (keywords-parser [a :default]))
(check-invalid-options (keywords-parser [a :default 1 oops]))
(check-invalid-options (keywords-parser [a :default 1 :default 2]))
(check-invalid-options (keywords-parser [a :default 1 :predicate]))
(check-invalid-options (keywords-parser [a :default 1 :predicate :default 2]))
(check-invalid-options (keywords-parser [a :default 1 :predicate null? oops]))
(check-invalid-options (keywords-parser [a :predicate]))
(check-invalid-options (keywords-parser [a :predicate null? :predicate string?]))
(check-invalid-options (keywords-parser [a :predicate null? :default 1 :predicate string?]))
(check-invalid-options (keywords-parser [a oops :default 1 :predicate null?]))
(check-invalid-options (keywords-parser [a :default 1 oops :predicate null?]))
(check-invalid-options (keywords-parser [a :boolean :boolean]))
(check-invalid-options (keywords-parser [a :default 1 :boolean]))
(check-invalid-options (keywords-parser [a :boolean :default 1]))
(check-invalid-options (keywords-parser [a :predicate null? :boolean]))
(check-invalid-options (keywords-parser [a :boolean :predicate null?]))
(check-invalid-options (keywords-parser [a :predicate null? :boolean :default 1]))
(check-invalid-options (keywords-parser [a :default 1 :boolean :predicate null?]))

;; option :default

(define parser2 (keywords-parser [a :default 1]))
(check-missing-value a (parser2 '(a)))
(check-values (parser2 '()) => 1 '())
(check-values (parser2 '(a a)) => 'a '())
(check-missing-value a (parser2 '(a a a)))
(check-values (parser2 '(b 2)) => 1 '(b 2))
(check-values (parser2 '(a 3)) => 3 '())
(check-values (parser2 '(b 2 a 3)) => 3 '(b 2))
(check-values (parser2 '(a a a 2)) => 2 '())
(check-values (parser2 '(a 1 a a)) => 'a '())
(check-values (parser2 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a 8)) 
              => 8 '(b 2 "foo" b 4 #\b #\a #\r c 6 b 7))
(check-missing-value a (parser2 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a)))

;; option :predicate

(define parser3 (keywords-parser [a :predicate number?]))
(check-missing-keyword a (parser3 '()))
(check-missing-keyword a (parser3 '(b 1)))
(check-missing-value a (parser3 '(a)))
(check-values (parser3 '(a 3)) => 3 '())
(check-pred-failed a number? (parser3 '(a "foo")))
(check-values (parser3 '(b 2 a 3)) => 3 '(b 2))
(check-values (parser3 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a 8)) 
              => 8 '(b 2 "foo" b 4 #\b #\a #\r c 6 b 7))
(check-pred-failed a number? 
                   (parser3 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a "8")))
(check-missing-value a (parser3 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a)))

;; options :default and :predicate

(define parser4 (keywords-parser [a :default "foo" :predicate char?]))
(check-missing-value a (parser4 '(a)))
(check-values (parser4 '()) => "foo" '())
(check-missing-value a (parser4 '(a a a)))
(check-values (parser4 '(b 2)) => "foo" '(b 2))
(check-values (parser4 '(a #\3)) => #\3 '())
(check-pred-failed a char? (parser4 '(a "foo")))
(check-values (parser4 '(b 2 a #\3)) => #\3 '(b 2))
(check-values (parser4 '(a a a #\2)) => #\2 '())
(check-pred-failed a char? (parser4 '(a 1 a a)))
(check-values (parser4 '(b 2 "foo" b 4 #\b #\a #\r c 6 b 7)) 
              => "foo" '(b 2 "foo" b 4 #\b #\a #\r c 6 b 7))
(check-values (parser4 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a #\8)) 
              => #\8 '(b 2 "foo" b 4 #\b #\a #\r c 6 b 7))
(check-pred-failed a char? 
                   (parser4 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a "8")))
(check-missing-value a (parser4 '(a 1 b 2 "foo" a 3 b 4 a a #\b #\a #\r c 6 b 7 a)))

;; option :boolean

(define parser5 (keywords-parser [a :boolean]))
(check-values (parser5 '()) => #f '())
(check-values (parser5 '(a)) => #t '())
(check-values (parser5 '(a a)) => #t '())
(check-values (parser5 '(a a a)) => #t '())
(check-values (parser5 '(a b a a c)) => #t '(b c))
(check-values (parser5 '(b c)) => #f '(b c))

;; multiple keywords

(define parser6 (keywords-parser [a :boolean]
                                 [b]
                                 [c :default 3]
                                 [d :predicate integer?]
                                 [e :predicate pair? :default '()]))
(check-missing-keyword b (parser6 '()))
(check-missing-keyword d (parser6 '(b 2)))
(check-missing-value b (parser6 '(b b b)))
(check-missing-value c (parser6 '(e X d X b X c)))
(check-missing-value d (parser6 '(e X d X c X b X d)))
(check-missing-value e (parser6 '(d X c X b X e)))
(check-pred-failed d integer? (parser6 '(b 2 d "foo")))
(check-pred-failed e pair? (parser6 '(d 4 e () b 2)))
(check-values (parser6 '(d 4 b foo)) => #f 'foo 3 4 '() '())
(check-values (parser6 '(d 4 a b foo e (x y z) c "bar")) 
              => #t 'foo "bar" 4 '(x y z) '())
(check-values (parser6 '(x d 4 a b foo y x e (x y z) c "bar" z y x a e (z . z) b bar z)) 
              => #t 'bar "bar" 4 '(z . z) '(x y x z y x z))
(check-pred-failed d integer?
                   (parser6 '(x a b foo y x e (x y z) d 4.1 c "bar" z y x a e (z . z) b bar z)))

;; letrec* semantics for evaluation of :default and :predicate expressions

(define parser7 (keywords-parser [a :default 1 :predicate number?]
                                 [b :default (number->string a) :predicate string?]
                                 [c :default (lambda () d) :predicate procedure?]
                                 [d]
                                 [e :default (string-append b b)]))
(check-values (let-values ([(a b c d e r) 
                            (parser7 '(d foo))])
                (values a b (c) d e r)) 
              => 1 "1" 'foo 'foo "11" '())
(check-values (let-values ([(a b c d e r) 
                            (parser7 (list "blah" 'c + 'x 'y 'd 'bar 'a 6/17 'Zzz))])
                (values a b (c) d e r)) 
              => 6/17 "6/17" 0 'bar "6/176/17" '("blah" x y Zzz))

(define parser8 (keywords-parser [a]
                                 [b :predicate (lambda (x)
                                                 (cond [(string? a) (integer? x)]
                                                       [(number? a) (char? x)]
                                                       [else #f]))]
                                 [c :predicate (lambda (x)
                                                 (equal? x b))]))
(check-values (parser8 '(c 2 b 2 a "A")) => "A" 2 2 '())
(check-pred-failed b (lambda (x)
                       (cond [(string? a) (integer? x)]
                             [(number? a) (char? x)]
                             [else #f]))
                   (parser8 '(c 2 b #\c a "A")))
(check-pred-failed b (lambda (x)
                       (cond [(string? a) (integer? x)]
                             [(number? a) (char? x)]
                             [else #f]))
                   (parser8 '(c 2 b 2 a X)))
(check-values (parser8 '(c #\c b #\c a 1)) => 1 #\c #\c '())
(check-pred-failed c (lambda (x)
                       (equal? x b))
                   (parser8 '(c X b 2 a "A")))
(check-pred-failed c (lambda (x)
                       (equal? x b))
                   (parser8 '(c X b #\c a 1)))

;; lambda/kw and case-lambda/kw

(check ((lambda/kw () 'ok)) => 'ok)
(check ((lambda/kw () 'ok) 1 'a 2 'b 3) => 'ok)
(check ((lambda/kw a a) 1 'a 2 'b 3) => '(1 a 2 b 3))
(check ((lambda/kw (x) x) 1 'a 2 'b 3) => 1)
(check ((lambda/kw (x y) (list x y)) 1 'a 2 'b 3) => '(1 a))
(check ((lambda/kw ([a]) a) 1 'a 2 'b 3) => 2)
(check ((lambda/kw ([a] [b]) (list a b)) 1 'c 4 'b 3 'a 2) => '(2 3))
(check ((lambda/kw ([a] [b] . r) (list a b r)) 1 'c 4 'b 3 'a 2) => '(2 3 (1 c 4)))
(check ((lambda/kw (x [a]) (list x a)) 1 'a 2 'b 3) => '(1 2))
(check ((lambda/kw (x y [a :default 5]) (list x y a)) 1 'a 2 'b 3) => '(1 a 5))
(check-missing-keyword a ((lambda/kw (x y [a]) #f) 1 'a 2 'b 3))
(check-missing-value b ((lambda/kw ([a] [b]) #f) 1 'a 2 'b))
(check-pred-failed a string? ((lambda/kw ([a :predicate string?]) #f) 1 'a 2 'b))
(check-invalid-formals (lambda/kw (["oops"]) 'ok))
(check-invalid-options (lambda/kw ([x :predicate]) 'ok))
(check ((lambda/kw (x y [a :boolean]
                        [b :default "foo"]
                        [c :default (lambda () d)]
                        [d :predicate char?])
          (list x y a b (c) d))
        1 2 'e 3 'd #\c 'f) 
       => '(1 2 #f "foo" #\c #\c))

(check-no-clause ((case-lambda/kw)))
(check-no-clause ((case-lambda/kw [(x y) 'first] [(x) 'second])))
(check ((case-lambda/kw [(x y) 'first] [(x) 'second]) 1 2 3) => 'first)
(check (guard (ex [else ex])
         ((case-lambda/kw [a (raise 'oops)]) 1 2 3))
       => 'oops)
(check (with-exception-handler
         (lambda (ex) 'ok)
         (lambda ()
           ((case-lambda/kw [a (raise-continuable 'oops)]) 1 2 3)))
       => 'ok)
(define f0
  (case-lambda/kw
    [(x [a :predicate string?] [b :default (string-length a)])
     (list x a b)]
    [([c] [d :boolean] . r)
     (list c d r)]
    [(x y z . r)
     (list x y z r)]))
(check-no-clause (f0 1 2))
(check-no-clause (f0 'a 'foo))
(check-no-clause (f0 'a "foo"))
(check (f0 'a 'foo 'c "bar") => '("bar" #f (a foo)))
(check (f0 1 'a "foo") => '(1 "foo" 3))
(check (f0 1 'b 'bar 'a "foo") => '(1 "foo" bar))
(check (f0 'a 'foo 'b 'bar) => '(a foo b (bar)))
(check (f0 'd 1 'c 2) => '(2 #t (1)))

;; define/kw

(define/kw (df0) #t)
(check (df0) => #t)
(check (df0 1 2 3) => #t)
(define/kw (df1 x y [a :boolean] [b :default (not a)] [c :predicate char?] . r)
  (list x y a b c r))
(check-missing-keyword c (df1 1 2 'b 3))
(check-missing-value b (df1 1 2 'c #\λ 'b))
(check-pred-failed c char? (df1 1 2 'c 'λ))
(check-invalid-formals (let () (define/kw (f x ["oops"]) #f) #f))
(check-invalid-options (let () (define/kw (f x [y :boolean :default 1]) #f) #f))
(check (df1 1 2 'd 3 'c #\λ 'e 'f) 
       => '(1 2 #f #t #\λ (d 3 e f)))
(check (df1 1 2 'c #\λ 'b 2) 
       => '(1 2 #f 2 #\λ ()))
(check (df1 1 2 'c #\λ 'a 2) 
       => '(1 2 #t #f #\λ (2)))


(check-report)
