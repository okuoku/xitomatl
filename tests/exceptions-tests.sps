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
  (xitomatl exceptions)
  (xitomatl srfi lightweight-testing))

(define-syntax check-ex
  (syntax-rules (?)
    [(_ expr ? pred)
     (check (call/cc
              (lambda (k)
                (with-exception-handler
                  (lambda (ex) (k (pred ex)))
                  (lambda () expr))))
            => #t)]))

;;; catch

;; basic
(check-ex (catch ex () (raise 1))
          ? (lambda (ex) (eqv? ex 1)))
(check (with-exception-handler
         (lambda (ex) 'R)
         (lambda ()
           (catch ex () (raise-continuable 1))))
       => 'R)
(check (catch ex ([else 'A 'B 'C]) (raise 1))
       => 'C)
(check (catch ex ([else 'A 'B 'C]) (raise-continuable 1))
       => 'C)
(check (catch ex ([(eqv? ex 1) 'bad]
                  [(and (warning? ex) 'ok) => (lambda (x) x)]
                  [else 'bad]) 
         (raise (make-warning)))
       => 'ok)
(check-ex (catch ex ([(error? ex) 'bad]
                     [(eqv? 1 ex) 'bad])
            (raise (make-warning))) 
          ? warning?)
(check (with-exception-handler
         (lambda (ex) 'R)
         (lambda ()
           (catch ex ([(error? ex) 'bad]
                      [(eqv? 1 ex) 'bad])
             (raise-continuable (make-warning)))))
       => 'R)
(check (catch ex ([(and (number? ex) (+ 1 ex))])
         (raise 1))
       => 2)
;; nested
(check (catch ex2 ([(null? ex2) 'bad]
                   [(eqv? ex2 1) 'foo 'bar 'ok])
         (catch ex1 ([(char? ex1) 'bad]
                     [(list? ex1) 'bad])
           (catch ex0 ([(string? ex0) 'bad])
             (raise 1))))
       => 'ok)
(check (with-exception-handler
         (lambda (ex) 'R)
         (lambda ()
           (catch ex2 ([(null? ex2) 'bad]
                       [(eqv? ex2 1) 'bad])
             (catch ex1 ([(char? ex1) 'bad]
                         [(list? ex1) 'bad])
               (catch ex0 ([(string? ex0) 'bad])
                 (raise-continuable 2))))))
       => 'R)
;; dynamic extent not exited and re-entered when re-raising
(let ([enters 0] [exits 0])
  (check-ex (catch ex ([(string? ex) 'bad])
              (dynamic-wind
                (lambda () (set! enters (+ 1 enters)))
                (lambda () (raise 1))
                (lambda () (set! exits (+ 1 exits))))) 
            ? (lambda (ex) (eqv? 1 ex)))
  (check enters => 1)
  (check exits => 1))
(let ([enters 0] [exits 0])
  (check (with-exception-handler
           (lambda (ex) 'R)
           (lambda ()
             (catch ex2 ([(null? ex2) 'bad]
                         [(eqv? ex2 1) 'bad])
               (catch ex1 ([(char? ex1) 'bad]
                           [(list? ex1) 'bad])
                 (catch ex0 ([(string? ex0) 'bad])
                   (dynamic-wind
                     (lambda () (set! enters (+ 1 enters)))
                     (lambda () (raise-continuable 2))
                     (lambda () (set! exits (+ 1 exits))))))))) 
         => 'R)
  (check enters => 1)
  (check exits => 1))
(let ([enters 0] [exits 0])
  (check (with-exception-handler
           (lambda (ex) 'R)
           (lambda ()
             (catch ex2 ([(null? ex2) 'bad]
                         [(eqv? ex2 1) 'bad])
               (catch ex1 ([(char? ex1) 'ok]
                           [(list? ex1) 'bad])
                 (catch ex0 ([(string? ex0) 'bad])
                   (dynamic-wind
                     (lambda () (set! enters (+ 1 enters)))
                     (lambda () (raise #\C))
                     (lambda () (set! exits (+ 1 exits))))))))) 
         => 'ok)
  (check enters => 1)
  (check exits => 1))

;;; warning

(check-ex (warning 'foo "bar") 
          ? warning?)
(check-ex (warning 'foo "bar")
          ? (lambda (ex) (not (serious-condition? ex))))
(check (catch ex ([(warning? ex) (list (condition-who ex) 
                                       (condition-message ex)
                                       (condition-irritants ex))])
         (warning 'someone "oops" 1 2))
       => '(someone "oops" (1 2)))
(check (with-exception-handler
         (lambda (ex) 'R)
         (lambda ()
           (warning 'someone "oops" 1 2)))
       => 'R)
;; test implementation's base exception handler returns for non-&serious
(check (begin (warning 'someone "oops" 1 2) 'R)
       => 'R)


(check-report)
