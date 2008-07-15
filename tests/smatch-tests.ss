#!/usr/bin/env scheme-script
#!r6rs
(import 
  (rnrs)
  (rnrs eval)
  (xitomatl smatch)
  (xitomatl srfi lightweight-testing)
  (xitomatl srfi cond-expand)
  (xitomatl irregex))

(define-syntax check-failed
  (syntax-rules ()
    [(_ expr) 
     (check (guard (ex [(assertion-violation? ex)
                        (and (message-condition? ex)
                             (who-condition? ex)
                             (list (condition-who ex) 
                                   (condition-message ex)))])
              (eval 'expr (environment '(rnrs) '(xitomatl smatch))))
            => '(smatch "failed to match"))]))

(cond-expand
  [ikarus
   (define-syntax check-dups-error
     (syntax-rules ()
       [(_ expr) 
        (check (guard (ex [(syntax-violation? ex)
                           (and (message-condition? ex)
                                (who-condition? ex)
                                (list (condition-who ex) 
                                      (condition-message ex)))])
                 (eval 'expr (environment '(rnrs) '(xitomatl smatch))))
               => '(smatch "duplicate identifier"))]))]
  [mzscheme
   (define-syntax check-dups-error
     (syntax-rules ()
       [(_ expr) 
        (check (guard (ex [(and (syntax-violation? ex) (message-condition? ex))
                           (and (irregex-search 
                                 "smatch: duplicate identifier"
                                 (condition-message ex))
                                #t)])
                 (eval 'expr (environment '(rnrs) '(xitomatl smatch))))
               => #t)]))])

;;; pairs / lists

(check (smatch '(a . b) [(x . y) (cons y x)]) => '(b . a))
(check-failed (smatch '(a . b) [(x) #f]))
(check (smatch '(lalala) [(x) (symbol? x) #t]) => #t)
(check-failed (smatch '(lalala) [(x) (number? x) #t]))
(check (smatch (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
         [(_ a b _ c _ _ d e f _ _ _ g) (list a b c d e f g)])
       => '(2 3 5 8 9 10 14))
(check (letrec ([flatten 
                 (lambda (l)
                   (smatch l
                     [((x . xr) . r) 
                      (or (pair? xr) (null? xr)) 
                      (append (flatten (cons x xr)) (flatten r))]
                     [(x . r) (cons (flatten x) (flatten r))]
                     [x x]))])
         (flatten '((a) ((b (c ((((d))) . z)) (((e) f)))))))       
       => '(a b c (d . z) e f))
(check-dups-error
  (smatch '(1 2 3 4) [(a b c a) (list a b c)]))

;;; vectors

(check (smatch (vector 'a "b" #\c) 
         [#(x y) #f]
         [#(x y z w) #f]
         [#(x "b" y) (cons y x)])
       => '(#\c . a))
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

;;; IrRegex regular expressions, 's mode so that "." matches newline

(check (smatch "a\nbcde\n\nf" 
         [(^$ "(a.?b)(.*)(\\n\n)(\\w)" a b _ d e) 
          (list a b d e)]) 
       => '("a\nbcde\n\nf" "a\nb" "\n\n" "f"))
(check-failed (smatch 'not-string
                [(^$ ".*") 'bad]))
(check (smatch 'not-string
         [(^$ ".*") 'bad]
         [x (symbol? x) 'ok])
       => 'ok)
(check-dups-error
  (smatch "((λ (x) (x x)) (λ (x) (x x)))" 
    [(^$ ".*(λ).*(λ).*" x y x) y]))
(check (smatch '(("parse me") "λλ")
         [(((^$ "(\\s*\\w+)+" a)) (^$ "\\S{0,3}" b)) 
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
         [(& A x y) 'bad]
         [_ 'opaque])
       => 'opaque)
(check (smatch c
         [(& C a b c d e) 'bad]
         [_ 'opaque])
       => 'opaque)
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

;;; not

(check (smatch 1 [(not 2) 'ok]) => 'ok)
(check-failed (smatch 1 [(not 1) 'bad]))
(check-dups-error (smatch 1 [(not (x x)) 'bad]))
(check (smatch '("fOO" #\c)
         [((not (^$ "F[Oo]*")) #\c) 'ok])
       => 'ok)

;;; everything

(check (smatch `(#("zzeee") 123 #((,b "fooo⁐⁐bar" (ign . #\c)) (a #(1 b #\p))))
         [(`#(,zzz)
           (not 124)
           #(((& B f "two" 3) 
              (^$ "fo(o*⁐{2}b)ar" _ m)
              ((and (not "ign") _) . x)) 
             ('a #(1 (and y (not 'B)) #\p))))
          (list f y x m)])
       => '(one b #\c "oo⁐⁐b"))

;;; smatch-lambda smatch-let and friends

(check ((smatch-lambda [123 #f] [_ #t]) 234) => #t)
(check ((smatch-lambda* [(x y . z) (reverse z)] [_ #f]) 1 2 3 4 5) => '(5 4 3))

(check (smatch-let ([(a b c) '(1 2 3)] 
                    [#(d (e (f))) '#(4 (5 (6)))])
         (define x 'asdf)
         (list a b c d e f x))
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


(check-report)
