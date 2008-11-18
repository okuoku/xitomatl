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
    symbol<?
    name=?
    non-empty-string?
    char-line-ending?
    library-name?
    list-of?
    #;improper-list?
    #;datum?)
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

  ;;--------------------------------------------------------------------------
  
  (define (symbol<? x y . r)
    (apply string<? (map symbol->string (cons* x y r))))
  
  (define (name=? x y . r)
    (apply symbol=? 
           (map (lambda (n) 
                  (cond [(identifier? n) (syntax->datum n)]
                        [(symbol? n) n]
                        [(string? n) (string->symbol n)]
                        [else (assertion-violation 'name=? 
                               "not an identifier, symbol, or string" n)]))
                (cons* x y r))))
  
  (define (non-empty-string? x)
    (and (string? x) (positive? (string-length x))))
  
  (define (char-line-ending? c)
    (and (memv c '(#\xa #\xd #\x85 #\x2028))  ;; correct? everything it should be?
         #t))
  
  (define (library-name? x)
    (and (list? x)
         (let loop ([l x] [is #F])
           (cond [(null? l) is]
                 [(and (symbol? (car l))
                       (positive? (string-length (symbol->string (car l))))) 
                  (loop (cdr l) #T)]
                 [(and (null? (cdr l))
                       (list? (car l))
                       (for-all exact-non-negative-integer? (car l)))
                  is]
                 [else #F]))))

  ;;--------------------------------------------------------------------------
  
  (define (list-of? pred)
    (letrec ([list-of?-pred
              (lambda (x)
                (cond [(pair? x) (and (pred (car x)) (list-of?-pred (cdr x)))]
                      [(null? x) #t]
                      [else #f]))])
      list-of?-pred))
  
  #;(define (improper-list? x)
    )
  
  #;(define (datum? x)
    ;; The naive implementation cannot handle cyclic structures.
    ;; How to do this..?
    )
  
)
