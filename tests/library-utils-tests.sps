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
  (xitomatl library-utils)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch))

(define-syntax check-AV
  (syntax-rules ()
    [(_ expr)
     (check (catch ex ([else (assertion-violation? ex)])
              expr
              'unexpected-return)
            => #T)]))

;; library-name-without-version
(check (library-name-without-version '(foo zab (0 4))) => '(foo zab))
(check (library-name-without-version '(foo zab ())) => '(foo zab))
(check (library-name-without-version '(foo zab)) => '(foo zab))
(check-AV (library-name-without-version 'oops))
(check-AV (library-name-without-version '(foo "oops")))
;; library-name-version
(check (library-name-version '(foo zab (0 4))) => '(0 4))
(check (library-name-version '(foo zab ())) => '())
(check (library-name-version '(foo zab)) => #F)
(check-AV (library-name-version 'oops))
(check-AV (library-name-version '(foo "oops")))
;; library-name?
(check (library-name? 1) => #F)
(check (library-name? '()) => #F)
(check (library-name? '("foo")) => #F)
(check (library-name? '(1)) => #F)
(check (library-name? '((1))) => #F)
(check (library-name? (list 'foo (string->symbol "") 'zab)) => #F)
(check (library-name? '(foo)) => #T)
(check (library-name? '(foo bar)) => #T)
(check (library-name? '(foo bar ())) => #T)
(check (library-name? '(foo bar zab blah asdf hoho)) => #T)
(check (library-name? '(foo bar "zab" blah asdf hoho)) => #F)
(check (library-name? '(foo (1))) => #T)
(check (library-name? '(foo bar)) => #T)
(check (library-name? '(foo bar (1))) => #T)
(check (library-name? '(foo bar (3 4))) => #T)
(check (library-name? '(foo bar zab (0 4 3 2 1 9 8))) => #T)
(check (library-name? '(foo bar (3 oops 4))) => #F)
(check (library-name? '(foo bar (3 4.1))) => #F)
(check (library-name? '(foo bar (3 -4 5))) => #F)
(check (library-name? '(foo bar (3 4) oops)) => #F)
;; library-name-symbol?
(check (library-name-symbol? 'a) => #T)
(check (library-name-symbol? 'abc) => #T)
(check (library-name-symbol? (string->symbol "")) => #F)
(check (library-name-symbol? "str") => #F)
(check (library-name-symbol? 1) => #F)
;; library-version?
(check (library-version? '()) => #T)
(check (library-version? '(1)) => #T)
(check (library-version? '(3 4)) => #T)
(check (library-version? '(0 4 3 2 1 9 8)) => #T)
(check (library-version? '(3 oops 4)) => #F)
(check (library-version? '(3 4.1)) => #F)
(check (library-version? '(3 -4 5)) => #F)
;; library-name<?
(check (library-name<?) => #T)
(check (library-name<? '(foo)) => #T)
(check (library-name<? '(bar) '(foo)) => #T)
(check (library-name<? '(foo) '(bar)) => #F)
(check (library-name<? '(foo) '(foo)) => #F)
(check (library-name<? '(foo) '(fooo)) => #T)
(check (library-name<? '(fooo) '(foo)) => #F)
(check (library-name<? '(foo) '(foo bar)) => #T)
(check (library-name<? '(foo bar) '(foo)) => #F)
(check (library-name<? '(foo bar) '(foo bar)) => #F)
(check (library-name<? '(foo bar) '(foo bar zab)) => #T)
(check (library-name<? '(foo bar zab) '(foo bar)) => #F)
(check (library-name<? '(foo bar zab) '(foo bar zab)) => #F)
(check (library-name<? '(foo bar zab) '(foo bar zaba)) => #T)
(check (library-name<? '(foo bar zaba) '(foo bar zab)) => #F)
(check (library-name<? '(bar foo) '(foo)) => #T)
(check (library-name<? '(foo) '(bar foo)) => #F)
(check (library-name<? '(foo) '(foo ())) => #T)
(check (library-name<? '(foo ()) '(foo)) => #F)
(check (library-name<? '(foo) '(foo (0))) => #T)
(check (library-name<? '(foo (0)) '(foo)) => #F)
(check (library-name<? '(foo (1)) '(foo (2))) => #T)
(check (library-name<? '(foo (2)) '(foo (1))) => #F)
(check (library-name<? '(foo (1)) '(foo (1))) => #F)
(check (library-name<? '(foo (2 3)) '(foo (3))) => #T)
(check (library-name<? '(foo (3)) '(foo (2 3))) => #F)
(check (library-name<? '(foo (1)) '(foo (1 0))) => #T)
(check (library-name<? '(foo (1 0)) '(foo (1))) => #F)
(check (library-name<? '(foo (1 2 3)) '(foo (1 2 3 0))) => #T)
(check (library-name<? '(foo (1 2 3 0)) '(foo (1 2 3))) => #F)
(check (library-name<? '(foo (3)) '(foo bar (2))) => #T)
(check (library-name<? '(foo bar (2)) '(foo (3))) => #F)
(check (library-name<? '(foo bar zab) '(foo bar zab (0 1 3))) => #T)
(check (library-name<? '(foo bar zab (0 1 3)) '(foo bar zab)) => #F)
(check (library-name<? '(foo bar zab (0 1 2)) '(foo bar zab (0 1 3))) => #T)
(check (library-name<? '(foo bar zab (0 1 3)) '(foo bar zab (0 1 2))) => #F)
(check (library-name<? '(foo bar zab (0 1 2)) '(foo bar zab (0 1 2))) => #F)
(check (library-name<? '(foo bar zab (0 1)) '(foo bar zab (0 1 2))) => #T)
(check (library-name<? '(foo bar zab (0 1 2)) '(foo bar zab (0 1))) => #F)
(check (library-name<? '(bar zab foo (1 2 3)) '(foo bar (0 1))) => #T)
(check (library-name<? '(foo bar (0 1)) '(bar zab foo (1 2 3))) => #F)
(check (library-name<? '(bar foo) '(foo (1 9)) '(foo (2)) '(fooo)) => #T)
(check (library-name<? '(bar foo) '(foo (1 9)) '(foo (2)) '(foo) '(zab)) => #F)
(check (list-sort library-name<?
                  '((foo bar) (aaaa aaa aa a) (z (13)) (z (1 2)) (bar foo)
                    (foo) (bar blah zab asdf (0)) (foo (0)) (foo bar (4 1))))
       => '((aaaa aaa aa a) (bar blah zab asdf (0)) (bar foo) (foo)
            (foo (0)) (foo bar) (foo bar (4 1)) (z (1 2)) (z (13))))
(check (library-name<? '(bar rab arb) '(foo (1 2 3)) '(foo (2)) '(foo arb)) => #T)
(check (library-name<? '(foo (2)) '(rab) '(foo arb)) => #F)
(check-AV (library-name<? '(foo bar) 'oops))
(check-AV (library-name<? '(foo "oops") '(zab)))
;; library-version<?
(check (library-version<? '() '(0)) => #T)
(check (library-version<? '(0) '()) => #F)
(check (library-version<? '() '()) => #F)
(check (library-version<? '(1 98 45) '(2 97)) => #T)
(check (library-version<? '(2 97) '(1 98 45)) => #F)
(check (library-version<? '(1 2 3) '(1 2 3 0)) => #T)
(check (library-version<? '(1 2 3 0) '(1 2 3)) => #F)
(check (library-version<? '(1 2 3 4) '(1 2 4 3)) => #T)
(check (library-version<? '(1 2 4 3) '(1 2 3 4)) => #F)
(check (library-version<? '() '(1 2 3) '(9 9) '(10)) => #T)
(check (library-version<? '() '(10) '(9 9)) => #F)
(check-AV (library-version<? '(1 2 3) 'oops))
(check-AV (library-version<? '(1 2 1.0) '(0 5)))


(check-report)
