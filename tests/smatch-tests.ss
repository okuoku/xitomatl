#!/usr/bin/env scheme-script
#!r6rs
(import 
  (rnrs)
  (rnrs eval)
  (xitomatl smatch)
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
              (eval 'expr (environment '(rnrs) '(xitomatl smatch) '(xitomatl irregex)))
              'unexpected-return)
            => '(smatch "failed to match"))]))

(define-syntax check-syntax-error/search-msg 
  (syntax-rules ()
    [(_ expr msg)
     (check (guard (ex 
                    [(and (syntax-violation? ex)
                          (message-condition? ex))
                     (and (irregex-search (irregex-quote msg) 
                                          (condition-message ex))
                          #t)])
              (eval 'expr (environment '(rnrs) '(xitomatl smatch)))
              'unexpected-return)
            => #t)]))

(define-syntax check-dups-error
  (syntax-rules ()
    [(_ expr) 
     (check-syntax-error/search-msg expr "duplicate")]))

(define-syntax check-vars-mismatch-error
  (syntax-rules ()
    [(_ expr) 
     (check-syntax-error/search-msg expr "or pattern variables mismatch")]))

(define-syntax check-contains-vars-error
  (syntax-rules ()
    [(_ expr) 
     (check-syntax-error/search-msg expr "not pattern contains variables")]))

;;; pairs / lists

(check (smatch '(a . b) [(x . y) (cons y x)]) => '(b . a))
(check (let-values ([vs (smatch '(a . b) [(x . y) (values y x)])])
         vs)
       => '(b a))
(check-failed (smatch '(a . b) [(x) #f]))
(check (smatch '(1 2 3 . 4) [(a b c . d) (list d c b a)]) => '(4 3 2 1))
(check-failed (smatch '(1 2 3 . 4) [(a b c) 'bad]))
(check (smatch '(lalala) [(x) (symbol? x) #t]) => #t)
(check-failed (smatch '(lalala) [(x) (number? x) #t]))
(check (smatch (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
         [(_ a b _ c _ _ d e f _ _ _ g) (list a b c d e f g)])
       => '(2 3 5 8 9 10 14))
(check (let flatten ([l '((a) ((b (c ((((d))) . z)) (((e) f)))))])
         (smatch l
           [((x . xr) . r) 
            (or (pair? xr) (null? xr)) 
            (append (flatten (cons x xr)) (flatten r))]
           [(x . r) (cons (flatten x) (flatten r))]
           [x x]))
       => '(a b c (d . z) e f))
(check-dups-error
  (smatch '(1 2 3 4) [(a b c a) (list a b c)]))

;;; vectors

(check (let-values ([vs (smatch (vector 'a "b" #\c) 
                          [#(x y) #f]
                          [#(x y z w) #f]
                          [#(x "b" y) 
                           (and (symbol? x) (symbol=? x 'a))
                           (values y x)])])
         vs)
       => '(#\c a))
(check-dups-error 
  (smatch (vector 1 2 3) [#(a a b) 'bad]))

;;; quoting

(check (smatch '(a . b) ['(a . b) #t]) => #t)
(check-failed (smatch '(a . b) ['(x . y) #t]))
(check (smatch '(foo bar baz) [('foo x 'baz) x]) => 'bar)

;;; quasiquoting
(define zzz "zzeee")
(check (smatch '(a . "zzeee") [`(a . ,zzz) #t]) => #t)

;;; constants

(check (smatch "asdf" ["asdf" #t]) => #t)
(check (smatch 123 [123 #t]) => #t)
(check (smatch #vu8(1 2) [#vu8(1 2) #t]) => #t)

;;; IrRegex regular expressions

(check (smatch "a\nbcde\n\nf" 
         [(^$ "(a.?b)(.*)(\\n\n)(\\w)" a b _ d e) 
          (list a b d e)]) 
       => '("a\nbcde\n\nf" "a\nb" "\n\n" "f"))
(check (smatch "a\nbcde\n\nf" 
         [(^$ '(seq (submatch (seq "a" (? any) "b")) (submatch (* any))
                    (submatch (seq #\linefeed "\n"))
                    (submatch (or alphanumeric ("_"))))
              a b _ d e) 
          (list a b d e)]) 
       => '("a\nbcde\n\nf" "a\nb" "\n\n" "f"))
(let ([irx (irregex "(a.?b)(.*)(\\n\n)(\\w)" 'single-line)])
  (check (smatch "a\nbcde\n\nf" 
           [(^$ irx a b _ d e) 
            (list a b d e)]) 
         => '("a\nbcde\n\nf" "a\nb" "\n\n" "f")))
(check-failed (smatch "a\nbcde\n\nf" 
                [(^$ '(seq (submatch (seq "a" (? nonl) "b")) (submatch (* nonl))
                           (submatch (seq #\linefeed "\n"))
                           (submatch (or alphanumeric ("_"))))
                     a b _ d e) 
                 (list a b d e)]))
(check-failed
 (let ([irx (irregex "(a.?b)(.*)(\\n\n)(\\w)")])
   (smatch "a\nbcde\n\nf" 
     [(^$ irx a b _ d e) 
      (list a b d e)])))
(check-failed (smatch 'not-string
                [(^$ ".*") 'bad]))
(check (smatch 'not-string
         [(^$ ".*") 'bad]
         [x (symbol? x) 'ok])
       => 'ok)
(check-failed (smatch "abcdefg" [(^$ "bcdefg") 'bad]))
(check-failed (smatch "abcdefg" [(^$ "abcdef") 'bad]))
(check-failed (smatch "abcdefg" [(^$ "bcdef") 'bad]))
(check-failed (smatch "abcdefg" [(^$ '(seq ("b") ("c") "de" "fg")) 'bad]))
(check-failed (smatch "abcdefg" [(^$ '(seq "ab" ("c") "de" ("f"))) 'bad]))
(check-failed (smatch "abcdefg" [(^$ '(seq ("b") ("c") "de" ("f"))) 'bad]))
(check-failed (let ([irx (irregex "bcdefg")])
                (smatch "abcdefg" [(^$ irx) 'bad])))
(check-failed (let ([irx (irregex "abcdef")])
                (smatch "abcdefg" [(^$ irx) 'bad])))
(check-failed (let ([irx (irregex "bcdef")])
                (smatch "abcdefg" [(^$ irx) 'bad])))
(check-dups-error
  (smatch "((λ (x) (x x)) (λ (x) (x x)))" 
    [(^$ ".*(λ).*(λ).*" x y x) y]))
(check (smatch '(("parse me") "λλ")
         [(((^$ "(\\s*\\w+)+" a)) (^$ "\\S{0,3}" b))
          (positive? (string-length b))
          (string-append a b)])
       => "parse meλλ")
(check (smatch '("foo bar" "bar baz")
         [123 'bad]
         [((^$ "(\\w+)\\s+(\\w+)" _ "foo" (^$ ".a." x)) 
           (^$ "(\\S+).*" _ (^$ "b.r" y)))
          (string=? x y)
          (string-append x y)])
       => "barbar")

;;; records

(define-record-type A (fields a (mutable b)))
(define-record-type B (parent A) (fields c))
(define-record-type C (opaque #t) (parent B) (fields d e))
(define a (make-A 1 2))
(define b (make-B 'one "two" 3))
(define c (make-C '#(a " ") 2 #\3 '(.4) "5"))

(check (smatch a [(& A x y) (list x y)])
       => '(1 2))
(check (smatch a 
         ['nope 'bad]
         [(& A 1 2) 'ok])
       => 'ok)
(check-failed 
 (let ()
   (define-record-type A (fields a (mutable b)))
   (define a (make-A 1 2))   
   (smatch a [(& A 3 2) 'bad])))
(check-dups-error 
 (let ()
   (define-record-type A (fields a (mutable b)))
   (define a (make-A 1 2))   
   (smatch a [(& A x x) 'bad])))
(check (smatch b
         ['nope 'bad]
         [(& A 'one x) (string? x) x])
       => "two")
(check-failed 
 (let ()
   (define-record-type A (fields a (mutable b)))
   (define-record-type B (parent A) (fields c))
   (define a (make-A 1 2))
   (define b (make-B 'one "two" 3))
   (smatch b
     ['nope 'bad]
     [(& A 'one x oops) 'bad])))
(check-dups-error 
 (let ()
   (define-record-type A (fields a (mutable b)))
   (define-record-type B (parent A) (fields c))
   (define a (make-A 1 2))
   (define b (make-B 'one "two" 3))
   (smatch (list 1 b 2)
     ['nope 'bad]
     [(x (& B x y z) z) 'bad])))
(check (smatch b
         ['nope 'bad]
         [(& B 'one (^$ "\\w{3}" x) 3) (string=? x "two") x]
         [x 'bad])
       => "two")
(check (smatch c
         [(& A x y) 'can-see-into-opaque]
         [_ 'opaque])
       => 'can-see-into-opaque)
(check (smatch c
         [(& C a b c d e) 'can-see-into-opaque]
         [_ 'opaque])
       => 'can-see-into-opaque)
(check (smatch c
         [(& (RTD (record-rtd a)) x y) (list x y)]
         [_ 'opaque])
       => '(#(a " ") 2))
(check (smatch c
         [(& (RTD (record-rtd b)) _ _ c) (char? c) c]
         [_ 'opaque])
       => #\3)
(check (smatch c
         [(& (RTD (record-type-descriptor C)) #('a (^$ "\\s+")) _ _ (n) "5")
          (and (number? n) (inexact? n))
          n]
         [_ 'opaque])
       => .4)

;;; and

(check (smatch 'foo [(and) 'ok]) => 'ok)
(check (smatch (list 1 2) [(and x y) (eq? x y)]) => #t)
(check (smatch '(1 2) 
         ['nope 'bad]
         [(and x (y z)) (list x y z)]
         ["nope" 'bad])
       => '((1 2) 1 2))
(check (smatch '(#(1) "foofoo")
         [(and l ((and v #(x)) (and s (^$ "(?:f(o)o)+" _ (and "o" ss)))))
          (list l v x s ss)])
       => '((#(1) "foofoo") #(1) 1 "foofoo" "o"))
(check-dups-error 
 (smatch '(#(1) "foofoo")
         [(and l ((and v #(x)) (and s (^$ "(?:f(o)o)+" _ (and "o" x)))))
          (list l v x s ss)]))
(check-failed 
 (smatch '(#(1) "foofoo")
         [(and l ((and v #(x)) (and s (^$ "(?:f(o)o)+" _ (and "nope" ss)))))
          (list l v x s ss)]))

;;; or

(check-failed (smatch 'foo [(or) 'bad]))
(check (smatch '(1 2)
         [(or (3 4) (1 2)) 'ok])
       => 'ok)
(check (smatch '(1 2)
         [(or 1 'a "foo") 'bad]
         [(or (3 x) (x 2)) x])
       => 1)
(check-failed (smatch '(1 2)
                [x (string? x) 'bad]
                [(or (3 x) (x 4)) x]))
(check-dups-error (smatch '(1 2)
                    [(or (x x) ('a x 'b x)) x]
                    [_ 'bad]))
(check-vars-mismatch-error 
 (smatch '(1 2)
   [(or (1 x) (y 4)) x]
   [_ 'bad]))
(check-vars-mismatch-error 
 (smatch '(1 2 3)
   [(or (2 x y) (y x 3)) x]
   [_ 'bad]))
(check (smatch '(1 2)
         [(or (_ _ x _ _) (x _ _) (_ x _) (_ _ x) (_ x)) x])
       => 2)
(check-vars-mismatch-error 
 (smatch '(1 2)
   [(or (_ _ x _ _) (x _ _) (_ x _) (x _ x) (_ x)) x]))
(check-vars-mismatch-error 
 (smatch '(1 2)
   [(or (_ _ x _ _) (x _ _) (_ x _) (_ _ y) (_ x)) x]))
(check (smatch '(1 2)
         [(or (or (or (_ _ x _ _) (x _ _))
                  (or (_ x _) (_ _ x)))
              (_ x))
          x])
       => 2)
(check-vars-mismatch-error
 (smatch '(1 2)
   [(or (or (or (_ _ x _ _) (x _ _))
            (or (_ x _) (_ x x)))
        (_ x))
    x]))
(check-vars-mismatch-error
 (smatch '(1 2)
   [(or (or (or (_ _ x _ _) (y _ _))
            (or (_ x _) (_ _ x)))
        (_ x))
    x]))
(check-vars-mismatch-error
 (smatch '(1 2)
   [(or (or (or (_ _ x _ _) (x _ _))
            (or (_ x _) (_ _ y)))
        (_ x))
    x]))
(check-vars-mismatch-error
 (smatch '(1 2)
   [(or (or (or (_ _ x _ _) (x _ _))
            (or (_ x _) (_ _ x)))
        (_ y))
    x]))
(check (smatch " \n asdfasdf"
         [(and x (or (& B _ y _) (^$ "(?:\\s*)((?:asdf){2})" _ 
                                     (or (and (_ y) (not _)) y))))
          x
          y])
       => "asdfasdf")
(check-vars-mismatch-error
 (smatch " \n asdfasdf"
   [(and x (or (& B _ y _) (^$ "(?:\\s*)((?:asdf){2})" _ z)))
    x
    z]))

;;; not

(check (smatch 1 [(not 2) 'ok]) => 'ok)
(check-failed (smatch 1 [(not 1) 'bad]))
(check-contains-vars-error (smatch 1 [(not (x x)) 'bad]))
(check (smatch '("fOO" #\c)
         [((not (^$ "F[Oo]*")) #\c) 'ok])
       => 'ok)

;;; everything

(check (let-values 
           ([vs
             (smatch `(#("zzeee") 123 #((,b "fooo⁐⁐bar" (ign . #\c)) (a #(1 b #\p))))
               [(`#(,zzz)
                 (not 124)
                 #(((& B f "two" 3) 
                    (^$ "fo(o*⁐{2}b)ar" _ m)
                    ((and (not "ign") _) . x)) 
                   ('a #(1 (and y (not 'B)) #\p))))
                (values f y x m)])])
         vs)
       => '(one b #\c "oo⁐⁐b"))

;;; smatch-lambda, smatch-let, and friends

(check ((smatch-lambda [123 #f] [_ #t]) 234) => #t)
(check ((smatch-lambda* [(x y . z) (reverse z)] [_ #f]) 1 2 3 4 5) => '(5 4 3))

(check (let-values ([vs (smatch-let ([(a b c) '(1 2 3)] 
                                     [#(d (e (f))) '#(4 (5 (6)))])
                          (define x 'asdf)
                          (values a b c d e f x))])
         vs)
       => '(1 2 3 4 5 6 asdf))
(check-dups-error 
 (smatch-let ([(a b c) '(1 2 3)]
              [#(d (b (f))) '#(4 (5 (6)))])
   (list a b c d f)))

(check (smatch-let* ([(a b) '(1 (2 3))]
                     [(a b) b]
                     (#(#(b) "s" a) `#(#(,(- a)) "s" ,(* 2 b))))
         (list a b))
       => '(6 -2))
(check-dups-error
 (smatch-let* ([(a b a) '(1 (2 3))]
               [(a b) b])
   (list a b)))

;;; arbitrary predicate pattern

(define (thing? obj)
  (smatch obj
    [#('foo (? string?)) #t]
    [#('foo (? string?) (? integer?) (? char?) (? thing?)) #t]
    [_ #f]))
(check (thing? '#(foo "bar" 1 #\2 #(foo "zab")))
       => #t)
(check (thing? '#(foo "bar" 1 #\2 #(foo "zab" blah)))
       => #f)
(check (thing? '#(foo "bar" .42 #\2 #(foo "zab")))
       => #f)


(check-report)
