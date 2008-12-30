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
(library (xitomatl predicates)
  (export
    non-negative-integer?
    exact-non-negative-integer?
    positive-integer?
    exact-positive-integer?
    exact-integer?
    list-of?
    #;datum?
    pairwise?
    symbol<?
    name=?
    non-empty-string?
    char-line-ending?
    library-name?
    library-name-symbol?
    library-version?
    library-name<?
    library-version<?)
  (import
    (rnrs))
  
  (define (non-negative-integer? x)
    (and (integer? x) (not (negative? x))))

  (define (exact-non-negative-integer? x)
    (and (integer? x) (exact? x) (not (negative? x))))
  
  (define (positive-integer? x)
    (and (integer? x) (positive? x)))
  
  (define (exact-positive-integer? x)
    (and (integer? x) (exact? x) (positive? x)))
  
  (define (exact-integer? x)
    (and (integer? x) (exact? x)))
  
  (define (list-of? pred)
    (letrec ([list-of?-pred
              (lambda (x)
                (if (pair? x)
                  (and (pred (car x))
                       (list-of?-pred (cdr x)))
                  (null? x)))])
      list-of?-pred))
  
  #;(define (datum? x)
    ;; The naive implementation cannot handle cyclic structures.
    ;; How to do this..?
    )

  (define pairwise?
    ;;; Make a predicate which tests if all its arguments are pairwise true
    ;;; for a given binary predicate.  0 and 1 arguments are always considered
    ;;; true; e.g.: ((pairwise? <)) => #T and ((pairwise? =) 42) => #T.
    ;;; The optional 2nd argument is an arbitrary procedure that takes 1
    ;;; argument, and it is applied to each element once and must return a value
    ;;; to use with the binary predicate, or raise an exception; this procedure
    ;;; is useful for efficiently type-checking elements and/or transforming them.
    (case-lambda
      [(binary-pred)
       (pairwise? binary-pred #F)]
      [(binary-pred proc)
       (let ([next (if proc
                     (lambda (l) (proc (car l)))
                     car)])
         (lambda args
           (or (null? args)
               (let ([x (next args)])
                 (let loop ([x x] [r (cdr args)])
                   (or (null? r)
                       (let ([y (next r)])
                         (and (binary-pred x y)
                              (loop y (cdr r))))))))))]))

  (define symbol<?
    (pairwise? string<?
     (lambda (x)
       (if (symbol? x)
         (symbol->string x)
         (assertion-violation 'symbol<? "not a symbol" x)))))

  (define name=?
    (pairwise? string=?
     (lambda (x) 
       (cond [(identifier? x) (symbol->string (syntax->datum x))]
             [(symbol? x) (symbol->string x)]
             [(string? x) x]
             [else (assertion-violation 'name=? 
                    "not an identifier, symbol, or string" x)]))))
  
  (define (non-empty-string? x)
    (and (string? x) (positive? (string-length x))))
  
  (define (char-line-ending? c)
    (and (memv c '(#\xa #\xd #\x85 #\x2028))  ;; correct? everything it should be?
         #t))
  
  (define (library-name? x)
    (and (pair? x)
         (library-name-symbol? (car x))
         (let loop ([x (cdr x)])
           (if (pair? x)
             (if (library-name-symbol? (car x))
               (loop (cdr x))
               (and (library-version? (car x))
                    (null? (cdr x))))
             (null? x)))))

  (define (library-name-symbol? x)
    (and (symbol? x)
         (positive? (string-length (symbol->string x)))))

  (define library-version? (list-of? exact-non-negative-integer?))

  (define library-name<?
    (pairwise?
     (letrec ([name<?
               (lambda (x y)
                 (if (pair? x)
                   (and (pair? y)
                        (if (symbol? (car x))
                          (and (symbol? (car y))
                               (or (symbol<? (car x) (car y))
                                   (and (symbol=? (car x) (car y))
                                        (name<? (cdr x) (cdr y)))))
                          (or (symbol? (car y))
                              (library-version<? (car x) (car y)))))
                   (pair? y)))])
       name<?)
     (lambda (x)
       (if (library-name? x)
         x
         (assertion-violation 'library-name<? "not a library name" x)))))

  (define library-version<?
    (pairwise?
     (letrec ([version<?
               (lambda (x y)
                 (if (pair? x)
                   (and (pair? y)
                        (or (< (car x) (car y))
                            (and (= (car x) (car y))
                                 (version<? (cdr x) (cdr y)))))
                   (pair? y)))])
       version<?)
     (lambda (x)
       (if (library-version? x)
         x
         (assertion-violation 'library-version<?
                              "not a library version" x)))))

)
