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
(library (xitomatl IU-match extras)
  (export
    match-lambda match-lambda/lexical-context
    match-lambda* match-lambda*/lexical-context
    match-let match-let/lexical-context
    match-let* match-let*/lexical-context    
    match-letrec match-letrec/lexical-context
    match-define match-define/lexical-context
    ;;; All of (xitomatl IU-match)
    match trace-match 
    match/lexical-context trace-match/lexical-context
    match-equality-test
    guard ... quasiquote unquote unquote-splicing)
  (import
    (rnrs)
    (xitomatl IU-match)
    (for (xitomatl IU-match macro-helpers) expand))
  
  (define-syntax match-lambda/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt clause ...)
         #'(lambda (x) (match/lexical-context ctxt x clause ...))])))
  
  (define-syntax match-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt clause ...)
         #'(match-lambda/lexical-context ctxt clause ...)])))
  
  (define-syntax match-lambda*/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt clause ...)
         #'(lambda x (match/lexical-context ctxt x clause ...))])))
  
  (define-syntax match-lambda*
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt clause ...)
         #'(match-lambda*/lexical-context ctxt clause ...)])))
  
  (define-syntax match-let/lexical-context
    (lambda (stx)
      (define (check-patterns pat*) 
        (check-ids/prevent-dups-across stx #'match-let pat*))
      (syntax-case stx ()
        [(_ ctxt named () body0 body* ...)
         #'(let named () body0 body* ...)]
        [(_ ctxt () body0 body* ...)
         #'(let () body0 body* ...)]
        [(_ ctxt named ([pat expr]) body0 body* ...)
         (check-patterns (list #'pat))
         #'(letrec ([named (lambda (t) (match/lexical-context ctxt t [pat body0 body* ...]))])
             (named expr))]
        [(_ ctxt named ([pat* expr*] ...) body0 body* ...)
         (check-patterns #'(pat* ...))
         (with-syntax ([(t* ...) (generate-temporaries #'(pat* ...))])
           #'(letrec ([named (lambda (t* ...) 
                               (match/lexical-context ctxt (vector t* ...) 
                                 [#(pat* ...) body0 body* ...]))])
               (named expr* ...)))]
        [(_ ctxt ([pat expr]) body0 body* ...)
         (check-patterns (list #'pat))
         #'(match/lexical-context ctxt expr [pat body0 body* ...])]
        [(_ ctxt ([pat* expr*] ...) body0 body* ...)
         (check-patterns #'(pat* ...))
         #'(match/lexical-context ctxt (vector expr* ...) [#(pat* ...) body0 body* ...])])))
  
  (define-syntax match-let
    (lambda (stx)
      (syntax-case stx ()
        [(_ named () body0 body* ...)
         (identifier? #'named)
         #'(let named () body0 body* ...)]
        [(_ () body0 body* ...)
         #'(let () body0 body* ...)]
        [(ctxt named ([pat* expr*] ...) body0 body* ...)
         (identifier? #'named)
         #'(match-let/lexical-context ctxt named ([pat* expr*] ...) body0 body* ...)]
        [(ctxt ([pat* expr*] ...) body0 body* ...)
         #'(match-let/lexical-context ctxt ([pat* expr*] ...) body0 body* ...)])))
  
  (define-syntax match-let*/lexical-context
    (lambda (stx)
      (define (check-patterns pat*)
        (check-ids/dups-okay stx #'match-let* pat*))
      (define (nest-matching s)
        (syntax-case s ()
          [(ctxt () body0 body* ...)
           #'(let () body0 body* ...)]
          [(ctxt ([pat0 expr0] [pat* expr*] ...) body0 body* ...)
           #`(match/lexical-context ctxt expr0
               [pat0 #,(nest-matching #'(ctxt ([pat* expr*] ...) body0 body* ...))])]))
      (syntax-case stx ()
        [(_ ctxt () body0 body* ...)
         #'(let () body0 body* ...)]
        [(_ ctxt ([pat* expr*] ...) body0 body* ...)
         (check-patterns #'(pat* ...))
         (nest-matching #'(ctxt ([pat* expr*] ...) body0 body* ...))])))
  
  (define-syntax match-let*
    (lambda (stx)
      (syntax-case stx ()
        [(_ () body0 body* ...)
         #'(let () body0 body* ...)]
        [(ctxt ([pat* expr*] ...) body0 body* ...)
         #'(match-let*/lexical-context ctxt ([pat* expr*] ...) body0 body* ...)])))
  
  (define-syntax match-letrec/lexical-context
    (lambda (stx)
      (define (check-patterns pat*) 
        (check-ids/prevent-dups-across stx #'match-letrec pat*))
      (syntax-case stx ()
        [(_ ctxt () body0 body* ...)
         #'(let () body0 body* ...)]
        [(_ ctxt ([pat* expr*] ...) body0 body* ...)
         (check-patterns #'(pat* ...))
         #'(let ()
             (match-define/lexical-context ctxt #(pat* ...) (vector expr* ...))
             body0 body* ...)])))
  
  (define-syntax match-letrec
    (lambda (stx)
      (syntax-case stx ()
        [(_ () body0 body* ...)
         #'(let () body0 body* ...)]
        [(ctxt ([pat* expr*] ...) body0 body* ...)
         #'(match-letrec/lexical-context ctxt ([pat* expr*] ...) body0 body* ...)])))
  
  (define-syntax match-define/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt pat expr)
         (with-syntax ([(id* ...) (find-ids/prevent-auto-recur stx #'match-define #'pat)])
           (with-syntax ([(t* ...) (generate-temporaries #'(id* ...))])
             #'(begin
                 (define t*) ...
                 (define dummy
                   (match/lexical-context ctxt expr [pat (set! t* id*) ... #f]))
                 (define id* 
                   (let ([v t*]) (set! t* #f) v)) 
                 ...)))])))
  
  (define-syntax match-define
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt pat expr)
         #'(match-define/lexical-context ctxt pat expr)])))
  
)
