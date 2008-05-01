#!/usr/bin/env scheme-script
#!r6rs
(import 
  (rnrs)
  (rnrs eval)
  (xitomatl smatch)
  (xitomatl srfi lightweight-testing))

(define-syntax check-failed
  (syntax-rules ()
    [(_ expr) 
     (check (guard (ex [(assertion-violation? ex)
                        (and (message-condition? ex) (condition-message ex))])
              (eval 'expr (environment '(rnrs) '(xitomatl smatch))))
            => "failed to match")]))

(define-syntax check-dups-error
  (syntax-rules ()
    [(_ expr) 
     (check (guard (ex [(syntax-violation? ex)
                        (and (message-condition? ex) (condition-message ex))])
              (eval 'expr (environment '(rnrs) '(xitomatl smatch))))
            => "duplicate identifier")]))

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

;;; quoting

(check (smatch '(a . b) ['(a . b) #t]) => #t)
(check-failed (smatch '(a . b) ['(x . y) #t]))
(check (smatch '(foo bar baz) [('foo x 'baz) x]) => 'bar)

;;; quasiquoting
(define zzz "zzeee")
(check (smatch '(a . "zzeee") [`(a . ,zzz) #t]) => #t)

;;; constants

(check (smatch "asdf" ["asdf" #t]) => #t)

;;; everything

(check (smatch '(#("zzeee") #(((ign . #\c)) (a #(1 b #\p))))
         [(`#(,zzz) #(((_ . x)) ('a #(1 y #\p))))
          (cons y x)])
       => '(b . #\c))

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
