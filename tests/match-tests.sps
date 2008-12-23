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

;;; BEWARE: The last section at the bottom of the file uses a lot of memory.
;;;         Comment it out if you don't have enough.  On 32-bit Ikarus,
;;;         it uses 800 MB.

#!r6rs
(import 
  (rnrs)
  (rnrs eval)
  (xitomatl match)
  (xitomatl srfi lightweight-testing)
  (xitomatl irregex))

(define-syntax check-failed
  (syntax-rules ()
    [(_ expr) 
     (check (guard (ex [(assertion-violation? ex)
                        (and (message-condition? ex)
                             (who-condition? ex)
                             (list (condition-who ex) 
                                   (condition-message ex)))])
              expr
              'unexpected-return)
            => '(match "failed to match"))]))

(define-syntax check-syntax-error/search-msg 
  (syntax-rules ()
    [(_ expr msg)
     (check (guard (ex 
                    [(and (syntax-violation? ex)
                          (message-condition? ex))
                     (and (irregex-search (irregex-quote msg) 
                                          (condition-message ex))
                          #t)])
              (eval 'expr (environment '(rnrs) '(xitomatl match)))
              'unexpected-return)
            => #t)]))

(define-syntax check-dups-error
  (syntax-rules ()
    [(_ expr) 
     (check-syntax-error/search-msg expr "duplicate identifier")]))

(define-syntax check-vars-mimatch-error
  (syntax-rules ()
    [(_ expr) 
     (check-syntax-error/search-msg expr ":or pattern variables mismatch")]))

(define-syntax check-contains-vars-error
  (syntax-rules ()
    [(_ expr) 
     (check-syntax-error/search-msg expr ":not pattern contains variables")]))

(define-syntax check-misuse-error
  (syntax-rules ()
    [(_ expr)
     (check-syntax-error/search-msg expr "misuse of pattern syntax")]))

;;; pairs / lists

(check (match '(a . b) [(x . y) (cons y x)]) => '(b . a))
(check (let-values ([vs (match '(a . b) [(x . y) (values y x)])])
         vs)
       => '(b a))
(check-failed (match '(a . b) [(x) #f]))
(check (match '(1 2 3 . 4) [(a b c . d) (list d c b a)]) => '(4 3 2 1))
(check-failed (match '(1 2 3 . 4) [(a b c) 'bad]))
(check (match '(lalala) [(x) (symbol? x) #t]) => #t)
(check-failed (match '(lalala) [(x) (number? x) #t]))
(check (match (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
         [(_ a b _ c _ _ d e f _ _ _ g) (list a b c d e f g)])
       => '(2 3 5 8 9 10 14))
(check (let flatten ([l '((a) ((b (c ((((d))) . z)) (((e) f)))))])
         (match l
           [((x . xr) . r) 
            (or (pair? xr) (null? xr)) 
            (append (flatten (cons x xr)) (flatten r))]
           [(x . r) (cons (flatten x) (flatten r))]
           [x x]))
       => '(a b c (d . z) e f))
(check-dups-error
 (match '(1 2 3 4) [(a b c a) (list a b c)]))

;;; vectors

(check (match '#() [#() 'ok]) => 'ok)
(check (match '#(1) [#(x) x]) => 1)
(check (let-values ([vs (match (vector 'a "b" #\c) 
                          [#(x y) #f]
                          [#(x y z w) #f]
                          [#(x "b" y) 
                           (and (symbol? x) (symbol=? x 'a))
                           (values y x)])])
         vs)
       => '(#\c a))
(check-dups-error 
 (match (vector 1 2 3) [#(a a b) 'bad]))

;;; quoting

(check-misuse-error (match '() [quote quote]))
(check-misuse-error (match '() [(1 . quote) quote]))
(check-misuse-error (match '() [(quote) quote]))
(check-misuse-error (match '() [((quote)) quote]))
(check-misuse-error (match '() [(a (quote)) quote]))
(check-misuse-error (match '() [((quote) b) quote]))
(check-misuse-error (match '() [(a (quote) b) quote]))
(check (match '(a . b) ['(a . b) #t]) => #t)
(check-failed (match '(a . b) ['(x . y) #t]))
(check (match '(foo bar baz) [('foo x 'baz) x]) => 'bar)
(check (match '(1 2 a b c) [(x y . '(a b c)) (list x y)]) => '(1 2))

;;; quasiquoting

(check-misuse-error (match '() [quasiquote quasiquote]))
(check-misuse-error (match '() [(1 . quasiquote) quasiquote]))
(check-misuse-error (match '() [(quasiquote) quasiquote]))
(check-misuse-error (match '() [((quasiquote)) quasiquote]))
(check-misuse-error (match '() [(a (quasiquote)) quasiquote]))
(check-misuse-error (match '() [((quasiquote) b) quasiquote]))
(check-misuse-error (match '() [(a (quasiquote) b) quasiquote]))
(define zzz "zzeee")
(check (match '(a . "zzeee") [`(a . ,zzz) #t]) => #t)
(check (match '(1 2 "zzeee" "zzeee") 
         [(x y . `(,zzz ,zzz))
          (list x y)]) 
       => '(1 2))

;;; constants

(check (match "asdf" ["asdf" #t]) => #t)
(check (match 123 [123 #t]) => #t)
(check (match #vu8(1 2) [#vu8(1 2) #t]) => #t)

;;; IrRegex regular expressions

(check-misuse-error (match '() [:regex :regex]))
(check-misuse-error (match '() [(1 . :regex) :regex]))
(check-misuse-error (match '() [(:regex) :regex]))
(check-misuse-error (match '() [((:regex)) :regex]))
(check-misuse-error (match '() [(a (:regex)) :regex]))
(check-misuse-error (match '() [((:regex) b) :regex]))
(check-misuse-error (match '() [(a (:regex) b) :regex]))
(check (match "a\nbcde\n\nf" 
         [(:regex "(a.?b)(.*)(\\n\n)(\\w)" b _ d e) 
          (list b d e)]) 
       => '("a\nb" "\n\n" "f"))
(check (match "a\nbcde\n\nf" 
         [(:regex '(seq (submatch (seq "a" (? any) "b")) (submatch (* any))
                        (submatch (seq #\linefeed "\n"))
                        (submatch (or alphanumeric ("_"))))
                  b _ d e) 
          (list b d e)]) 
       => '("a\nb" "\n\n" "f"))
(let ([irx (irregex "(a.?b)(.*)(\\n\n)(\\w)" 'single-line)])
  (check (match "a\nbcde\n\nf" 
           [(:regex irx b _ d e) 
            (list b d e)]) 
         => '("a\nb" "\n\n" "f")))
(check-failed (match "a\nbcde\n\nf" 
                [(:regex '(seq (submatch (seq "a" (? nonl) "b")) (submatch (* nonl))
                               (submatch (seq #\linefeed "\n"))
                               (submatch (or alphanumeric ("_"))))
                         b _ d e) 
                 (list b d e)]))
(check-failed
 (let ([irx (irregex "(a.?b)(.*)(\\n\n)(\\w)")])
   (match "a\nbcde\n\nf" 
     [(:regex irx b _ d e) 
      (list b d e)])))
(check-failed (match 'not-string
                [(:regex ".*") 'bad]))
(check (match 'not-string
         [(:regex ".*") 'bad]
         [x (symbol? x) 'ok])
       => 'ok)
(check-failed (match "abcdefg" [(:regex "bcdefg") 'bad]))
(check-failed (match "abcdefg" [(:regex "abcdef") 'bad]))
(check-failed (match "abcdefg" [(:regex "bcdef") 'bad]))
(check-failed (match "abcdefg" [(:regex '(seq ("b") ("c") "de" "fg")) 'bad]))
(check-failed (match "abcdefg" [(:regex '(seq "ab" ("c") "de" ("f"))) 'bad]))
(check-failed (match "abcdefg" [(:regex '(seq ("b") ("c") "de" ("f"))) 'bad]))
(check-failed (let ([irx (irregex "bcdefg")])
                (match "abcdefg" [(:regex irx) 'bad])))
(check-failed (let ([irx (irregex "abcdef")])
                (match "abcdefg" [(:regex irx) 'bad])))
(check-failed (let ([irx (irregex "bcdef")])
                (match "abcdefg" [(:regex irx) 'bad])))
(check-dups-error
 (match "((λ (x) (x x)) (λ (x) (x x)))" 
   [(:regex ".*(λ).*(λ).*" x x) x]))
(check (match '(("parse me") "λλ")
         [(((:and (:regex "(?:\\s*\\w+)+") a)) (:and b (:regex "\\S{0,3}")))
          (positive? (string-length b))
          (string-append a b)])
       => "parse meλλ")
(check (match '("foo bar" "bar baz")
         [123 'bad]
         [((:regex "(\\w+)\\s+(\\w+)" "foo" (:and x (:regex ".a."))) 
           (:regex "(\\S+).*" (:and (:regex "b.r") y)))
          (string=? x y)
          (string-append x y)])
       => "barbar")
(check-failed (match "asdf"
                [(:regex ".*" x) x]))
(check-failed (match "asdf"
                [(:regex ".*" x y z) (list x y z)]))
(check-failed (match "foo bar"
                [(:regex "(\\w+)\\s+(\\w+)" _ _ x y)
                 (list x y)]))

;;; symbols against regular expressions (uses :regex pattern logic)

(check-misuse-error (match '() [:symbol 1]))
(check-misuse-error (match '() [(a (:symbol) b) 1]))
(check (match 'foobar [(:symbol "fo+\\s*bar") 'ok]) => 'ok)
(check (match 'foooo__bar [(:symbol "f(o+)(\\S*)bar" 'oooo '__) 'ok]) => 'ok)
(check (match 'foozab [(:symbol "foo(?:(bar)|zab)" #F) 'ok]) => 'ok)
(check-failed (match 'foobar [(:symbol "fo+\\s*bar" #F _ x) 'bad]))
(check-failed (match 'foooo__bar [(:symbol "f(o+)(\\S*)bar" _) 'bad]))

;;; records

(check-misuse-error (match '() [:record :record]))
(check-misuse-error (match '() [(1 . :record) :record]))
(check-misuse-error (match '() [(:record) :record]))
(check-misuse-error (match '() [(:record 2 3) :record]))
(check-misuse-error (match '() [((:record)) :record]))
(check-misuse-error (match '() [(a (:record)) :record]))
(check-misuse-error (match '() [((:record) b) :record]))
(check-misuse-error (match '() [(a (:record) b) :record]))
(define-record-type A (fields a (mutable b)))
(define-record-type B (parent A) (fields c))
(define-record-type C (opaque #t) (parent B) (fields d e))
(define a (make-A 1 2))
(define b (make-B 'one "two" 3))
(define c (make-C '#(a " ") 2 #\3 '(.4) "5"))

(check (match a [(:record A x y) (list x y)])
       => '(1 2))
(check (match a 
         ['nope 'bad]
         [(:record A 1 2) 'ok])
       => 'ok)
(check-failed 
 (let ()
   (define-record-type A (fields a (mutable b)))
   (define a (make-A 1 2))   
   (match a [(:record A 3 2) 'bad])))
(check-dups-error 
 (let ()
   (define-record-type A (fields a (mutable b)))
   (define a (make-A 1 2))   
   (match a [(:record A x x) 'bad])))
(check (match b
         ['nope 'bad]
         [(:record A 'one x) (string? x) x])
       => "two")
(check-failed 
 (let ()
   (define-record-type A (fields a (mutable b)))
   (define-record-type B (parent A) (fields c))
   (define a (make-A 1 2))
   (define b (make-B 'one "two" 3))
   (match b
     ['nope 'bad]
     [(:record A 'one x oops) 'bad])))
(check-dups-error 
 (let ()
   (define-record-type A (fields a (mutable b)))
   (define-record-type B (parent A) (fields c))
   (define a (make-A 1 2))
   (define b (make-B 'one "two" 3))
   (match (list 1 b 2)
     ['nope 'bad]
     [(x (:record B x y z) z) 'bad])))
(check (match b
         ['nope 'bad]
         [(:record B 'one (:and (:regex "\\w{3}") x) 3) (string=? x "two") x]
         [x 'bad])
       => "two")
(check (match c
         [(:record A x y) 'can-see-into-opaque]
         [_ 'opaque])
       => 'can-see-into-opaque)
(check (match c
         [(:record C a b c d e) 'can-see-into-opaque]
         [_ 'opaque])
       => 'can-see-into-opaque)
(check (match c
         [(:record (RTD (record-rtd a)) x y) (list x y)]
         [_ 'opaque])
       => '(#(a " ") 2))
(check (match c
         [(:record (RTD (record-rtd b)) _ _ c) (char? c) c]
         [_ 'opaque])
       => #\3)
(check (match c
         [(:record (RTD (record-type-descriptor C)) #('a (:regex "\\s+")) _ _ (n) "5")
          (and (number? n) (inexact? n))
          n]
         [_ 'opaque])
       => .4)

;;; and

(check-misuse-error (match '() [:and :and]))
(check-misuse-error (match '() [(1 . :and) :and]))
(check (match 'foo [(:and) 'ok]) => 'ok)
(check (match (list 1 2) [(:and x y) (eq? x y)]) => #t)
(check (match '(1 2) 
         ['nope 'bad]
         [(:and x (y z)) (list x y z)]
         ["nope" 'bad])
       => '((1 2) 1 2))
(check (match '(#(1) "foofoo")
         [(:and l ((:and v #(x)) (:and s (:regex "(?:f(o)o)+" (:and "o" ss)))))
          (list l v x s ss)])
       => '((#(1) "foofoo") #(1) 1 "foofoo" "o"))
(check-dups-error 
 (match '(#(1) "foofoo")
   [(:and l ((:and v #(x)) (:and s (:regex "(?:f(o)o)+" (:and "o" x)))))
    (list l v x s ss)]))
(check-failed 
 (match '(#(1) "foofoo")
   [(:and l ((:and v #(x)) (:and s (:regex "(?:f(o)o)+" (:and "nope" ss)))))
    (list l v x s ss)]))

;;; or

(check-misuse-error (match '() [:or :or]))
(check-misuse-error (match '() [(1 . :or) :or]))
(check-failed (match 'foo [(:or) 'bad]))
(check (match '(1 2)
         [(:or (3 4) (1 2)) 'ok])
       => 'ok)
(check (match '(1 2)
         [(:or 1 'a "foo") 'bad]
         [(:or (3 x) (x 2)) x])
       => 1)
(check-failed (match '(1 2)
                [x (string? x) 'bad]
                [(:or (3 x) (x 4)) x]))
(check-dups-error (match '(1 2)
                    [(:or (x x) ('a x 'b x)) x]
                    [_ 'bad]))
(check-vars-mimatch-error 
 (match '(1 2)
   [(:or (1 x) (y 4)) x]
   [_ 'bad]))
(check-vars-mimatch-error 
 (match '(1 2 3)
   [(:or (2 x y) (y x 3)) x]
   [_ 'bad]))
(check (match '(1 2)
         [(:or (_ _ x _ _) (x _ _) (_ x _) (_ _ x) (_ x)) x])
       => 2)
(check-vars-mimatch-error 
 (match '(1 2)
   [(:or (_ _ x _ _) (x _ _) (_ x _) (x _ x) (_ x)) x]))
(check-vars-mimatch-error 
 (match '(1 2)
   [(:or (_ _ x _ _) (x _ _) (_ x _) (_ _ y) (_ x)) x]))
(check (match '(1 2)
         [(:or (:or (:or (_ _ x _ _) (x _ _))
                    (:or (_ x _) (_ _ x)))
               (_ x))
          x])
       => 2)
(check-vars-mimatch-error
 (match '(1 2)
   [(:or (:or (:or (_ _ x _ _) (x _ _))
              (:or (_ x _) (_ x x)))
         (_ x))
    x]))
(check-vars-mimatch-error
 (match '(1 2)
   [(:or (:or (:or (_ _ x _ _) (y _ _))
              (:or (_ x _) (_ _ x)))
         (_ x))
    x]))
(check-vars-mimatch-error
 (match '(1 2)
   [(:or (:or (:or (_ _ x _ _) (x _ _))
              (:or (_ x _) (_ _ y)))
         (_ x))
    x]))
(check-vars-mimatch-error
 (match '(1 2)
   [(:or (:or (:or (_ _ x _ _) (x _ _))
              (:or (_ x _) (_ _ x)))
         (_ y))
    x]))
(check (match " \n asdfasdf"
         [(:and x (:or (:record B _ y _) (:regex "(?:\\s*)((?:asdf){2})" 
                                                 (:or (:and (_ y) (:not _)) y))))
          x
          y])
       => "asdfasdf")
(check-vars-mimatch-error
 (match " \n asdfasdf"
   [(:and x (:or (:record B _ y _) (:regex "(?:\\s*)((?:asdf){2})" z)))
    x
    z]))

;;; not

(check-misuse-error (match '() [:not :not]))
(check-misuse-error (match '() [(1 . :not) not]))
(check-misuse-error (match '() [(:not) not]))
(check-misuse-error (match '() [(:not 2 3) not]))
(check-misuse-error (match '() [((:not)) not]))
(check-misuse-error (match '() [(a (:not)) not]))
(check-misuse-error (match '() [((:not) b) not]))
(check-misuse-error (match '() [(a (:not) b) not]))
(check (match 1 [(:not 2) 'ok]) => 'ok)
(check-failed (match 1 [(:not 1) 'bad]))
(check-contains-vars-error (match 1 [(:not (x x)) 'bad]))
(check (match '("fOO" #\c)
         [((:not (:regex "F[Oo]*")) #\c) 'ok])
       => 'ok)

;;; everything

(check (let-values 
           ([vs
             (match `(#("zzeee") 123 #((,b "fooo⁐⁐bar" (ign . #\c)) (a #(1 b #\p))))
               [(`#(,zzz)
                 (:not 124)
                 #(((:record B f "two" 3) 
                    (:regex "fo(o*⁐{2}b)ar" m)
                    ((:and (:not "ign") _) . x)) 
                   ('a #(1 (:and y (:not 'B)) #\p))))
                (values f y x m)])])
         vs)
       => '(one b #\c "oo⁐⁐b"))

;;; matches?, match-lambda, match-let, and friends

(check ((matches? 1) 1) => #t)
(check ((matches? (x . y)) '(1 2 3)) => #t)
(check ((matches? (x ... . r)) 'any) => #t)
(check ((matches? (:or 1 (:regex "\\s+"))) "foo") => #f)
(check ((matches? (:and _ (:predicate string?))) 1) => #f)
(check ((match-lambda [123 #f] [_ #t]) 234) => #t)
(check ((match-lambda* [(x y . z) (reverse z)] [_ #f]) 1 2 3 4 5) => '(5 4 3))

(check (let-values ([vs (match-let ([(a b c) '(1 2 3)] 
                                    [#(d (e (f))) '#(4 (5 (6)))])
                          (define x 'asdf)
                          (values a b c d e f x))])
         vs)
       => '(1 2 3 4 5 6 asdf))
(check-dups-error 
 (match-let ([(a b c) '(1 2 3)]
             [#(d (b (f))) '#(4 (5 (6)))])
   (list a b c d f)))

(check (match-let* ([(a b) '(1 (2 3))]
                    [(a b) b]
                    [#(#(b) "s" a) `#(#(,(- a)) "s" ,(* 2 b))])
         (list a b))
       => '(6 -2))
(check-dups-error
 (match-let* ([(a b a) '(1 (2 3))]
              [(a b) b])
   (list a b)))

;;; arbitrary predicate pattern

(check-misuse-error (match '() [:predicate :predicate]))
(check-misuse-error (match '() [(1 . :predicate) :predicate]))
(check-misuse-error (match '() [(:predicate) :predicate]))
(check-misuse-error (match '() [(:predicate 2 3) :predicate]))
(check-misuse-error (match '() [((:predicate)) :predicate]))
(check-misuse-error (match '() [(a (:predicate)) :predicate]))
(check-misuse-error (match '() [((:predicate) b) :predicate]))
(check-misuse-error (match '() [(a (:predicate) b) :predicate]))
(define (thing? obj)
  (match obj
    [#('foo (:predicate string?)) #t]
    [#('foo (:predicate string?) (:predicate integer?) 
            (:predicate char?) (:predicate thing?))
     #t]
    [_ #f]))
(check (thing? '#(foo "bar" 1 #\2 #(foo "zab")))
       => #t)
(check (thing? '#(foo "bar" 1 #\2 #(foo "zab" blah)))
       => #f)
(check (thing? '#(foo "bar" .42 #\2 #(foo "zab")))
       => #f)

;;; multiple list / improper list elements ... pattern

(check-misuse-error (match 1 [... ...]))
(check-misuse-error (match 1 [(x . ...) x]))
(check-misuse-error (match '(1) [(...) ...]))
(check-misuse-error (match '(1 (2) 3) [(x (...) y) (list x ... y)]))
(check (match '() [(x ...) x])
       => '())
(check-failed (match 'foo [(x ...) x]))
(check (match 'foo [(x ... . r) (list x r)]) 
       => '(() foo))
(check-dups-error (match 'foo [(x ... . x) 'bad]))
(check (match '(1) [(x ...) x])
       => '(1))
(check (match '(1 2) [(x ...) x])
       => '(1 2))
(check (match '(1 2 3) [(x ...) x])
       => '(1 2 3))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(x ...) x])
       => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(check-failed (match '() [(x ... a) (list x a)]))
(check (match '(1) [(x ... a) (list x a)])
       => '(() 1))
(check (match '(1 2) [(x ... a) (list x a)])
       => '((1) 2))
(check (match '(1 2 3) [(x ... a) (list x a)])
       => '((1 2) 3))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(x ... a) (list x a)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) 20))
(check-failed (match '() [(x ... a b) (list x a b)]))
(check-failed (match '(1) [(x ... a b) (list x a b)]))
(check (match '(1 2) [(x ... a b) (list x a b)])
       => '(() 1 2))
(check (match '(1 2 3) [(x ... a b) (list x a b)])
       => '((1) 2 3))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(x ... a b) (list x a b)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18) 19 20))
(check-failed (match '() [(x ... a b c) (list x a b c)]))
(check-failed (match '(1) [(x ... a b c) (list x a b c)]))
(check-failed (match '(1 2) [(x ... a b c) (list x a b c)]))
(check (match '(1 2 3) [(x ... a b c) (list x a b c)])
       => '(() 1 2 3))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(x ... a b c) (list x a b c)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17) 18 19 20))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(x ... a b c . r) (list x a b c r)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17) 18 19 20 ()))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 . #()) 
         [(x ... a b c . r) (list x a b c r)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17) 18 19 20 #()))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 . #(16 17 18 19 20)) 
         [(x ... a b c . r) (list x a b c r)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12) 13 14 15 #(16 17 18 19 20)))
(check-failed (match '() [(a x ...) (list a x)]))
(check-failed (match 'foo [(a x ...) (list a x)]))
(check-failed (match 'foo [(a x ... . r) (list a x r)]))
(check (match '(1) [(a x ...) (list a x)])
       => '(1 ()))
(check (match '(1 2) [(a x ...) (list a x)])
       => '(1 (2)))
(check (match '(1 2 3) [(a x ...) (list a x)])
       => '(1 (2 3)))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(a x ...) (list a x)])
       => '(1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
(check-failed (match '(1 2 3 4 5 6 7) [(0 x ...) (list a b x)]))
(check-failed (match '(1 2 3 4 5 6 7) [(a b c 0 x ...) (list a b x)]))
(check-failed (match '(1 2 3 4 5 6 7) [(0 x ... 7) (list a b x)]))
(check-failed (match '(1 2 3 4 5 6 7) [(a b c 0 x ... 6 7) (list a b x)]))
(check-failed (match '() [(a x ... b) (list a x b)]))
(check-failed (match '(1) [(a x ... b) (list a x b)]))
(check (match '(1 2) [(a x ... b) (list a x b)])
       => '(1 () 2))
(check (match '(1 2 3) [(a x ... b) (list a x b)])
       => '(1 (2) 3))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(a x ... b) (list a x b)])
       => '(1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) 20))
(check-failed (match '() [(a x ... b c) (list a x b c)]))
(check-failed (match '(1) [(a x ... b c) (list a x b c)]))
(check-failed (match '(1 2) [(a x ... b c) (list a x b c)]))
(check (match '(1 2 3) [(a x ... b c) (list a x b c)])
       => '(1 () 2 3))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(a x ... b c) (list a x b c)])
       => '(1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18) 19 20))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(a b c x ... d e f . r) (list a b c x d e f r)])
       => '(1 2 3 (4 5 6 7 8 9 10 11 12 13 14 15 16 17) 18 19 20 ()))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 . #()) 
         [(a b c x ... d e f . r) (list a b c x d e f r)])
       => '(1 2 3 (4 5 6 7 8 9 10 11 12 13 14 15 16 17) 18 19 20 #()))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 . #(16 17 18 19 20)) 
         [(a b c x ... d e f . r) (list a b c x d e f r)])
       => '(1 2 3 (4 5 6 7 8 9 10 11 12) 13 14 15 #(16 17 18 19 20)))
(check (match '() [((x ...) ...) x])
       => '())
(check-dups-error (match '() [((x ...) x ...) 'bad]))
(check (match 'foo [((x ...) ... . r) (list x r)])
       => '(() foo))
(check-failed (match '(1 2) [((x ...) ...) x]))
(check (match '((1)) [((x ...) ...) x])
       => '((1)))
(check (match '((1 2) (3 4) (5 6)) [((x y) ...) (list x y)])
       => '((1 3 5) (2 4 6)))
(check-failed (match '((1 2) (3 4) (5 6 7)) [((x y) ...) (list x y)]))
(check (match '((1) (2 3) (4 5 6) (7 8 9 10)) [((x ... y) ...) (list x y)])
       => '((() (2) (4 5) (7 8 9)) (1 3 6 10)))
(check (match '() [(((x ...) ...) ...) x])
       => '())
(check-vars-mimatch-error (match '() [(((:or (x ...) (x y ...)) ...) ...) 'bad]))
(check (match '(1 2 3 4 5)
         [(:or ((:and (:predicate symbol?) x) ...)
               ((:and (:predicate number?) x) ...))
          x]) 
       => '(1 2 3 4 5))
(check-failed (match 'foo [(((x ...) ...) ...) x]))
(check (match '((foo) (bar baz)) 
         [(((x ... . r) ...) ...) (list x r)])
       => '(((()) (() ())) ((foo) (bar baz))))
(check (match '(((1 2 3) (4 5) (6)) 
                ((a) (b c) (d e f) (g h i k))
                (("foo" "bar") () () ("baz"))) 
         [(((x ...) ...) ...) x])
       => '(((1 2 3) (4 5) (6)) 
            ((a) (b c) (d e f) (g h i k))
            (("foo" "bar") () () ("baz"))))
(check-failed (match '(((1 2 3) (4 5) (6)) 
                       ((a) (b c) (d e f) (g h i k))
                       (("foo" "bar") () () ("baz"))) 
                [(((a x ...) ...) ...) x]))
(check-failed (match '(((1 2 3) (4 5) (6)) 
                       ((a) (b c) (d e f) (g h i k))
                       (("foo" "bar") () () ("baz"))) 
                [(((x ... a) ...) ...) x]))
(check (match '(#(1 2) #(3 4)) [(#(a b) ...) (list a b)])
       => '((1 3) (2 4)))
(check (match '("foo bar" "hoho ho" "asdf blah")
         [((:regex "(\\w+)\\s+(\\w+)" a b) ...)
          (list a b)])
       => '(("foo" "hoho" "asdf") ("bar" "ho" "blah")))
(check (match (list (make-A 1 2) (make-B #\1 #\2 #\3) (make-C "1" "2" "3" "4" "5"))
         [((:record A x y) ...) (list x y)])
       => '((1 #\1 "1") (2 #\2 "2")))
(check (match '(1 2 3 4 5) [(x ... y ...) (list x y)])
       => '((1 2 3 4 5) ()))
(check (match '(1 2 3 4 5) [(x ... y ... z ...) (list x y z)])
       => '((1 2 3 4 5) () ()))
(check (match '(1 2 3 4 5) [(x ... 2 y ...) (list x y)])
       => '((1) (3 4 5)))
(check (match '(1 2 3 4 5) [(x ... 1 y ...) (list x y)])
       => '(() (2 3 4 5)))
(check (match '(1 2 3 2 4 2 5 2 6) [(x ... 2 y ...) (list x y)])
       => '((1 2 3 2 4 2 5) (6)))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [(x ... 5 y ... 13 z ...) (list x y z)])
       => '((1 2 3 4) (6 7 8 9 10 11 12) (14 15 16 17 18 19 20)))
(check (match '(1 5 2 13 3 5 13 4 5 6 7 8 9 13 10 11 12 13 14 15 16 17 18 19 20) 
         [(x ... 5 y ... 13 z ...) (list x y z)])
       => '((1 5 2 13 3 5 13 4) (6 7 8 9 13 10 11 12) (14 15 16 17 18 19 20)))
(check (match (list (make-A 1 2) (make-B #\1 #\2 #\3) (make-C "1" "2" "3" "4" "5")
                    (make-B 'one 'two 'three))
         [((:record A a b) ... (:record B _ _ (:regex "\\d")) (:record A c d) ...) 
          (list a b c d)])
       => '((1 #\1) (2 #\2) (one) (two)))
(check-misuse-error (match '() [(x (... 1.0)) x]))
(check-misuse-error (match '() [(x (... -1)) x]))
(check-misuse-error (match '() [(x (... oops)) x]))
(check-misuse-error (match '() [(x (... 0 0)) x]))
(check-misuse-error (match '() [(x (... 3 2)) x]))
(check-misuse-error (match '() [(x (... 0 1.0)) x]))
(check-misuse-error (match '() [(x (... 0 oops)) x]))
(check-misuse-error (match '() [(... 1) ...]))
(check-misuse-error (match '() [(... 1 2) ...]))
(check-misuse-error (match '() [((... 1) y) ...]))
(check-misuse-error (match '() [((... 1 2) y) ...]))
(check-failed (match '() [(x (... 1)) x]))
(check-failed (match '(1 2 3 4) [(x (... 0 3)) x]))
(check (match '(1 2 3) [(x (... 1)) x])
       => '(1 2 3))
(check (match '(1 2 3) [(x (... 0 3)) x])
       => '(1 2 3))
(check (match '(((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))
         [((:and a ((:and b (x (... 1 3))) (... 1 2))) (... 3 3))
          (list a b x)])
       => '((((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))
            (((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))
            (((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))))
(check-failed (match '(((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))
                [((:and a ((:and b (x (... 2 3))) (... 1 2))) (... 3 3))
                 (list a b x)]))
(check-failed (match '(((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11) (12)))
                [((:and a ((:and b (x (... 1 3))) (... 1 2))) (... 3 3))
                 (list a b x)]))
(check-failed (match '(((4 5 6)) ((7 8) (9 10 11) (12)))
                [((:and a ((:and b (x (... 1 3))) (... 1 2))) (... 3 3))
                 (list a b x)]))
(check (match '(1 2 3 4 5 6 7 8 9)
         [(a b x ... y (... 3))
          (list a b x y)])
       => '(1 2 (3 4 5 6) (7 8 9)))
(check (match '(1 2 3 4 5 6 7 8 9 10 11 12 13 . foo)
         [(a x (... 4 4) 6 7 y (... 6) . r)
          (list a x y r)])
       => '(1 (2 3 4 5) (8 9 10 11 12 13) foo))
(check-failed (match '(1 2 3 4 5 6 7 8 9 10 11 12 13  . foo)
                [(a x (... 4 4) 6 7 y (... 7) . r)
                 (list a x y r)]))
(check-failed (match '(1 2 3 4 5 6 7 8 9 10 11 12 13  . foo)
                [(x (... 4 4) 6 7 y (... 6) . r)
                 (list x y r)]))
(check-failed (match '(1 2 3 4 5 6 7 8 9 10 11 12 13  . foo)
                [(a b x (... 4) 6 7 y (... 6) . r)
                 (list a b x y r)]))

;;; multiple vector elements ... pattern

(check-misuse-error (match '#(1) [#(...) ...]))
(check-misuse-error (match '(1 #(2) 3) [(x #(...) y) (list x ... y)]))
(check (match '#() [#(x ...) x])
       => '())
(check-failed (match 'foo [#(x ...) x]))
(check-failed (match '(foo) [#(x ...) x]))
(check-dups-error (match 'foo [#(x ... x) 'bad]))
(check (match '#(1) [#(x ...) x])
       => '(1))
(check (match '#(1 2) [#(x ...) x])
       => '(1 2))
(check (match '#(1 2 3) [#(x ...) x])
       => '(1 2 3))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(x ...) x])
       => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(check-failed (match '#() [#(x ... a) (list x a)]))
(check (match '#(1) [#(x ... a) (list x a)])
       => '(() 1))
(check (match '#(1 2) [#(x ... a) (list x a)])
       => '((1) 2))
(check (match '#(1 2 3) [#(x ... a) (list x a)])
       => '((1 2) 3))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(x ... a) (list x a)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) 20))
(check-failed (match '#() [#(x ... a b) (list x a b)]))
(check-failed (match '#(1) [#(x ... a b) (list x a b)]))
(check (match '#(1 2) [#(x ... a b) (list x a b)])
       => '(() 1 2))
(check (match '#(1 2 3) [#(x ... a b) (list x a b)])
       => '((1) 2 3))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(x ... a b) (list x a b)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18) 19 20))
(check-failed (match '#() [#(x ... a b c) (list x a b c)]))
(check-failed (match '#(1) [#(x ... a b c) (list x a b c)]))
(check-failed (match '#(1 2) [#(x ... a b c) (list x a b c)]))
(check (match '#(1 2 3) [#(x ... a b c) (list x a b c)])
       => '(() 1 2 3))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(x ... a b c) (list x a b c)])
       => '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17) 18 19 20))
(check-failed (match '#() [#(a x ...) (list a x)]))
(check-failed (match 'foo [#(a x ...) (list a x)]))
(check (match '#(1) [#(a x ...) (list a x)])
       => '(1 ()))
(check (match '#(1 2) [#(a x ...) (list a x)])
       => '(1 (2)))
(check (match '#(1 2 3) [#(a x ...) (list a x)])
       => '(1 (2 3)))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(a x ...) (list a x)])
       => '(1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
(check-failed (match '#(1 2 3 4 5 6 7) [#(0 x ...) (list a b x)]))
(check-failed (match '#(1 2 3 4 5 6 7) [#(a b c 0 x ...) (list a b x)]))
(check-failed (match '#(1 2 3 4 5 6 7) [#(0 x ... 7) (list a b x)]))
(check-failed (match '#(1 2 3 4 5 6 7) [#(a b c 0 x ... 6 7) (list a b x)]))
(check-failed (match '#() [#(a x ... b) (list a x b)]))
(check-failed (match '#(1) [#(a x ... b) (list a x b)]))
(check (match '#(1 2) [#(a x ... b) (list a x b)])
       => '(1 () 2))
(check (match '#(1 2 3) [#(a x ... b) (list a x b)])
       => '(1 (2) 3))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(a x ... b) (list a x b)])
       => '(1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) 20))
(check-failed (match '#() [#(a x ... b c) (list a x b c)]))
(check-failed (match '#(1) [#(a x ... b c) (list a x b c)]))
(check-failed (match '#(1 2) [#(a x ... b c) (list a x b c)]))
(check (match '#(1 2 3) [#(a x ... b c) (list a x b c)])
       => '(1 () 2 3))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(a x ... b c) (list a x b c)])
       => '(1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18) 19 20))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(a b c x ... d e f) (list a b c x d e f)])
       => '(1 2 3 (4 5 6 7 8 9 10 11 12 13 14 15 16 17) 18 19 20))
(check (match '#() [#((x ...) ...) x])
       => '())
(check-dups-error (match '#() [#((x ...) x ...) 'bad]))
(check-failed (match '#(1 2) [#((x ...) ...) x]))
(check (match '#((1)) [#((x ...) ...) x])
       => '((1)))
(check (match '#((1 2) (3 4) (5 6)) [#((x y) ...) (list x y)])
       => '((1 3 5) (2 4 6)))
(check-failed (match '#((1 2) (3 4) (5 6 7)) [#((x y) ...) (list x y)]))
(check (match '#((1) (2 3) (4 5 6) (7 8 9 10)) [#((x ... y) ...) (list x y)])
       => '((() (2) (4 5) (7 8 9)) (1 3 6 10)))
(check (match '(#(1) #(2 3) #(4 5 6) #(7 8 9 10)) [(#(x ... y) ...) (list x y)])
       => '((() (2) (4 5) (7 8 9)) (1 3 6 10)))
(check (match '#() [#(((x ...) ...) ...) x])
       => '())
(check-vars-mimatch-error (match '#() [#(#((:or #(x ...) #(x y ...)) ...) ...) 'bad]))
(check (match '#(1 2 3 4 5)
         [(:or #((:and (:predicate symbol?) x) ...)
               #((:and (:predicate number?) x) ...))
          x]) 
       => '(1 2 3 4 5))
(check-failed (match 'foo [#(((x ...) ...) ...) x]))
(check (match '#(((1 2 3) (4 5) (6)) 
                 ((a) (b c) (d e f) (g h i k))
                 (("foo" "bar") () () ("baz"))) 
         [#(((x ...) ...) ...) x])
       => '(((1 2 3) (4 5) (6)) 
            ((a) (b c) (d e f) (g h i k))
            (("foo" "bar") () () ("baz"))))
(check-failed (match '#(((1 2 3) (4 5) (6)) 
                        ((a) (b c) (d e f) (g h i k))
                        (("foo" "bar") () () ("baz"))) 
                [#(((a x ...) ...) ...) x]))
(check-failed (match '#(((1 2 3) (4 5) (6)) 
                        ((a) (b c) (d e f) (g h i k))
                        (("foo" "bar") () () ("baz"))) 
                [#(((x ... a) ...) ...) x]))
(check (match '#(#(1 2) #(3 4)) [#(#(a b) ...) (list a b)])
       => '((1 3) (2 4)))
(check (match '#("foo bar" "hoho ho" "asdf blah")
         [#((:regex "(\\w+)\\s+(\\w+)" a b) ...)
          (list a b)])
       => '(("foo" "hoho" "asdf") ("bar" "ho" "blah")))
(check (match (vector (make-A 1 2) (make-B #\1 #\2 #\3) (make-C "1" "2" "3" "4" "5"))
         [#((:record A x y) ...) (list x y)])
       => '((1 #\1 "1") (2 #\2 "2")))
(check (match '#(1 2 3 4 5) [#(x ... y ...) (list x y)])
       => '((1 2 3 4 5) ()))
(check (match '#(1 2 3 4 5) [#(x ... y ... z ...) (list x y z)])
       => '((1 2 3 4 5) () ()))
(check (match '#(1 2 3 4 5) [#(x ... 2 y ...) (list x y)])
       => '((1) (3 4 5)))
(check (match '#(1 2 3 4 5) [#(x ... 1 y ...) (list x y)])
       => '(() (2 3 4 5)))
(check (match '#(1 2 3 2 4 2 5 2 6) [#(x ... 2 y ...) (list x y)])
       => '((1 2 3 2 4 2 5) (6)))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 
         [#(x ... 5 y ... 13 z ...) (list x y z)])
       => '((1 2 3 4) (6 7 8 9 10 11 12) (14 15 16 17 18 19 20)))
(check (match '#(1 5 2 13 3 5 13 4 5 6 7 8 9 13 10 11 12 13 14 15 16 17 18 19 20) 
         [#(x ... 5 y ... 13 z ...) (list x y z)])
       => '((1 5 2 13 3 5 13 4) (6 7 8 9 13 10 11 12) (14 15 16 17 18 19 20)))
(check (match (vector (make-A 1 2) (make-B #\1 #\2 #\3) (make-C "1" "2" "3" "4" "5")
                      (make-B 'one 'two 'three))
         [#((:record A a b) ... (:record B _ _ (:regex "\\d")) (:record A c d) ...) 
          (list a b c d)])
       => '((1 #\1) (2 #\2) (one) (two)))
(check-misuse-error (match '#() [#(x (... 1.0)) x]))
(check-misuse-error (match '#() [#(x (... -1)) x]))
(check-misuse-error (match '#() [#(x (... oops)) x]))
(check-misuse-error (match '#() [#(x (... 0 0)) x]))
(check-misuse-error (match '#() [#(x (... 3 2)) x]))
(check-misuse-error (match '#() [#(x (... 0 1.0)) x]))
(check-misuse-error (match '#() [#(x (... 0 oops)) x]))
(check-misuse-error (match '#() [#(... 1) ...]))
(check-misuse-error (match '#() [#(... 1 2) ...]))
(check-misuse-error (match '#() [#((... 1) y) ...]))
(check-misuse-error (match '#() [#((... 1 2) y) ...]))
(check-failed (match '#() [#(x (... 1)) x]))
(check-failed (match '#(1 2 3 4) [#(x (... 0 3)) x]))
(check (match '#(1 2 3) [#(x (... 1)) x])
       => '(1 2 3))
(check (match '#(1 2 3) [#(x (... 0 3)) x])
       => '(1 2 3))
(check (match '#(((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))
         [#((:and a ((:and b (x (... 1 3))) (... 1 2))) (... 3 3))
          (list a b x)])
       => '((((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))
            (((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))
            (((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))))
(check-failed (match '#(((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11)))
                [#((:and a ((:and b (x (... 2 3))) (... 1 2))) (... 3 3))
                 (list a b x)]))
(check-failed (match '#(((1) (2 3)) ((4 5 6)) ((7 8) (9 10 11) (12)))
                [#((:and a ((:and b (x (... 1 3))) (... 1 2))) (... 3 3))
                 (list a b x)]))
(check-failed (match '#(((4 5 6)) ((7 8) (9 10 11) (12)))
                [#((:and a ((:and b (x (... 1 3))) (... 1 2))) (... 3 3))
                 (list a b x)]))
(check (match '#(1 2 3 4 5 6 7 8 9)
         [#(a b x ... y (... 3))
          (list a b x y)])
       => '(1 2 (3 4 5 6) (7 8 9)))
(check (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13)
         [#(a x (... 4 4) 6 7 y (... 6))
          (list a x y)])
       => '(1 (2 3 4 5) (8 9 10 11 12 13)))
(check-failed (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13)
                [#(a x (... 4 4) 6 7 y (... 7))
                 (list a x y)]))
(check-failed (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13)
                [#(x (... 4 4) 6 7 y (... 6))
                 (list x y)]))
(check-failed (match '#(1 2 3 4 5 6 7 8 9 10 11 12 13)
                [#(a b x (... 4) 6 7 y (... 6))
                 (list a b x y)]))

;;; Huge sized

(begin  ;; #; comment-out this begin if you don't have enough memory.
(define size #e1e7)
(define l 
  (do ([i size (- i 1)]
       [l '() (cons i l)])
    [(= i 0) l]))
(check (match l [(x ...) (length x)]) => size)
(check (match l 
         [(a x ...) 
          (list a (length x))])
       => `(1 ,(- size 1)))
(check (match l
         [(x ... a)
          (list (length x) a)])
       => `(,(- size 1) ,size))
(check (match l
         [(x ... a b)
          (list (length x) a b)])
       => `(,(- size 2) ,(- size 1) ,size))
(check (match l
         [(a b x ... c d e)
          (list a b (length x) c d e)])
       => `(1 2 ,(- size 5) ,(- size 2) ,(- size 1) ,size))
(check (match l
         [(a b c d e f g h i j k l m n o p q r s t u v w x y z 
           A B C D E F G H I J K L M N O P Q R S T U V W X Y Z ooo ...)
          (list a b c d e f g h i j k l m n o p q r s t u v w x y z
                A B C D E F G H I J K L M N O P Q R S T U V W X Y Z (length ooo))])
       => `(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
            24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42
            43 44 45 46 47 48 49 50 51 52 ,(- size 52)))
(check (match l
         [(ooo ... a b c d e f g h i j k l m n o p q r s t u v w x y z 
           A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
          (list (length ooo) a b c d e f g h i j k l m n o p q r s t u v w x y z
                A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)])
       => `(,(- size 52) ,(- size 51) ,(- size 50) ,(- size 49) ,(- size 48)
                         ,(- size 47) ,(- size 46) ,(- size 45) ,(- size 44)
                         ,(- size 43) ,(- size 42) ,(- size 41) ,(- size 40)
                         ,(- size 39) ,(- size 38) ,(- size 37) ,(- size 36)
                         ,(- size 35) ,(- size 34) ,(- size 33) ,(- size 32)
                         ,(- size 31) ,(- size 30) ,(- size 29) ,(- size 28)
                         ,(- size 27) ,(- size 26) ,(- size 25) ,(- size 24)
                         ,(- size 23) ,(- size 22) ,(- size 21) ,(- size 20)
                         ,(- size 19) ,(- size 18) ,(- size 17) ,(- size 16)
                         ,(- size 15) ,(- size 14) ,(- size 13) ,(- size 12)
                         ,(- size 11) ,(- size 10) ,(- size 9) ,(- size 8)
                         ,(- size 7) ,(- size 6) ,(- size 5) ,(- size 4)
                         ,(- size 3) ,(- size 2) ,(- size 1) ,size))
(check-failed (match l [(x (... 10000001)) x]))
(check-failed (match l [(x (... 12345 5432123)) x]))
(check (match l 
         [(x ... 1234567 y ...)
          (list (length x) (length y))])
       => `(1234566 ,(- size 1234567)))
(check (match l 
         [(x ... 999 1000 1001 y ... 6225192 6225193 z ...)
          (list (length x) (length y) (length z))])
       => `(998 6224190 ,(- size 6225193)))
(set! l #f)  ;; free memory

(define v (make-vector size))
(do ([i size (- i 1)])
  [(= i 0)]
  (vector-set! v (- i 1) i))
(check (match v [#(x ...) (length x)]) => size)
(check (match v 
         [#(a x ...) 
          (list a (length x))])
       => `(1 ,(- size 1)))
(check (match v
         [#(x ... a)
          (list (length x) a)])
       => `(,(- size 1) ,size))
(check (match v
         [#(x ... a b)
          (list (length x) a b)])
       => `(,(- size 2) ,(- size 1) ,size))
(check (match v
         [#(a b x ... c d e)
          (list a b (length x) c d e)])
       => `(1 2 ,(- size 5) ,(- size 2) ,(- size 1) ,size))
(check (match v
         [#(a b c d e f g h i j k l m n o p q r s t u v w x y z 
            A B C D E F G H I J K L M N O P Q R S T U V W X Y Z ooo ...)
          (list a b c d e f g h i j k l m n o p q r s t u v w x y z
                A B C D E F G H I J K L M N O P Q R S T U V W X Y Z (length ooo))])
       => `(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
            24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42
            43 44 45 46 47 48 49 50 51 52 ,(- size 52)))
(check (match v
         [#(ooo ... a b c d e f g h i j k l m n o p q r s t u v w x y z 
            A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
          (list (length ooo) a b c d e f g h i j k l m n o p q r s t u v w x y z
                A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)])
       => `(,(- size 52) ,(- size 51) ,(- size 50) ,(- size 49) ,(- size 48)
                         ,(- size 47) ,(- size 46) ,(- size 45) ,(- size 44)
                         ,(- size 43) ,(- size 42) ,(- size 41) ,(- size 40)
                         ,(- size 39) ,(- size 38) ,(- size 37) ,(- size 36)
                         ,(- size 35) ,(- size 34) ,(- size 33) ,(- size 32)
                         ,(- size 31) ,(- size 30) ,(- size 29) ,(- size 28)
                         ,(- size 27) ,(- size 26) ,(- size 25) ,(- size 24)
                         ,(- size 23) ,(- size 22) ,(- size 21) ,(- size 20)
                         ,(- size 19) ,(- size 18) ,(- size 17) ,(- size 16)
                         ,(- size 15) ,(- size 14) ,(- size 13) ,(- size 12)
                         ,(- size 11) ,(- size 10) ,(- size 9) ,(- size 8)
                         ,(- size 7) ,(- size 6) ,(- size 5) ,(- size 4)
                         ,(- size 3) ,(- size 2) ,(- size 1) ,size))
(check-failed (match v [#(x (... 10000001)) x]))
(check-failed (match v [#(x (... 12345 5432123)) x]))
(check (match v 
         [#(x ... 1234567 y ...)
          (list (length x) (length y))])
       => `(1234566 ,(- size 1234567)))
(check (match v 
         [#(x ... 999 1000 1001 y ... 6225192 6225193 z ...)
          (list (length x) (length y) (length z))])
       => `(998 6224190 ,(- size 6225193)))
)


(check-report)
