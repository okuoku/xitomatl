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
  (xitomatl predicates)
  (xitomatl srfi lightweight-testing))

;; non-negative-integer?
(check (non-negative-integer? "no") => #F)
(check (non-negative-integer? 1/2) => #F)
(check (non-negative-integer? -2) => #F)
(check (non-negative-integer? 0) => #T)
(check (non-negative-integer? 321) => #T)
(check (non-negative-integer? 7.0) => #T)
;; exact-non-negative-integer?
(check (exact-non-negative-integer? #\z) => #F)
(check (exact-non-negative-integer? 0.0) => #F)
(check (exact-non-negative-integer? 45.0) => #F)
(check (exact-non-negative-integer? -34) => #F)
(check (exact-non-negative-integer? 0) => #T)
(check (exact-non-negative-integer? 89348) => #T)
;; positive-integer?
(check (positive-integer? '(nope)) => #F)
(check (positive-integer? 0) => #F)
(check (positive-integer? -34) => #F)
(check (positive-integer? 789/23) => #F)
(check (positive-integer? 85.0) => #T)
(check (positive-integer? 3916237) => #T)
;; exact-positive-integer?
(check (exact-positive-integer? '#()) => #F)
(check (exact-positive-integer? 21.0) => #F)
(check (exact-positive-integer? -9) => #F)
(check (exact-positive-integer? 3.21) => #F)
(check (exact-positive-integer? 1) => #T)
(check (exact-positive-integer? 567/1) => #T)
;; exact-integer?
(check (exact-integer? '()) => #F)
(check (exact-integer? 1.0) => #F)
(check (exact-integer? 3/2) => #F)
(check (exact-integer? 0) => #T)
(check (exact-integer? -76) => #T)
(check (exact-integer? 238) => #T)
;; symbol<?
(check (symbol<? 'blah 'asdf) => #F)
(check (symbol<? 'f 'h) => #T)
(check (symbol<? 'aaa 'aab 'ab 'ba 'br 'ha 'zdsf) => #T)
;; name=?
(check (name=? #'foo 'foo) => #T)
(check (name=? #'foo 'foo "foo" #'foo "foo" 'foo) => #T)
(check (name=? #'foo 'bar) => #F)
(check (name=? "foo" #'foo 'foo 'bar) => #F)
;; non-empty-string?
(check (non-empty-string? "") => #F)
(check (non-empty-string? "a") => #T)
(check (non-empty-string? "blah asdf zab") => #T)
;; char-line-ending?
(check (char-line-ending? 'nope) => #F)
(check (char-line-ending? #\a) => #F)
(check (char-line-ending? #\xa) => #T)
(check (char-line-ending? #\xd) => #T)
(check (char-line-ending? #\x85) => #T)
(check (char-line-ending? #\x2028) => #T)
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
(check (library-name? '(foo bar (1))) => #T)
(check (library-name? '(foo bar (3 4))) => #T)
(check (library-name? '(foo bar (5 4 3 2 1 9 8))) => #T)
(check (library-name? '(foo bar (3 oops 4))) => #F)
(check (library-name? '(foo bar (3 4.1))) => #F)
(check (library-name? '(foo bar (3 -4 5))) => #F)
(check (library-name? '(foo bar (3 4) oops)) => #F)
;; list-of?
(check ((list-of? string?) '()) => #T)
(check ((list-of? number?) '(-45/6 23.71)) => #T)
(check ((list-of? (lambda (x) #F)) '(hehe)) => #F)
(check ((list-of? char?) '(#\a #\b "c" #\d)) => #F)
;; TODO: improper-list?
;; TODO: datum?


(check-report)
