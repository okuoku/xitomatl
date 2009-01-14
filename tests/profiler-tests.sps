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
  (srfi :78 lightweight-testing)
  (xitomatl profiler meta)
  (xitomatl profiler srfi-time)
  (only (srfi :19 time) time?))

(define-syntax check-values
  (syntax-rules (=>)
    [(_ expr => vals ...)
     (check (let-values ([v expr]) v) => (list vals ...))]))

(define clp0 
  (case-lambda/profiled
     [(x) (values (+ x x) (- x))]
     [(x y) (* x y)]))
(define lp0
  (lambda/profiled a 
    (apply values (reverse a))))
(define/profiled (dp0 x y . a)
  (if (number? x)
    (apply + x y a)
    (begin ((call/cc (lambda (cc) (set! dp0-k cc) values)) 'C)
           'R)))
(define dp0-k)
(define/profiled (unused . _) (assert #F))

(check-values (clp0 123) => 246 -123)
(check (clp0 4 3) => 12)
(check-values (lp0 'x 1 'y 2) => 2 'y 1 'x)
(check-values (lp0 1 2) => 2 1)
(check (lp0 1) => 1)
(check (dp0 1 2 3 4 5) => 15)
(check (dp0 'x 'y) => 'R)
(check (call/cc dp0-k) => 'C)

(let ([keys (vector->list (hashtable-keys (profiled-procedures-HT)))])
  (check (and (memq clp0 keys) #T) => #T)
  (check (and (memq lp0 keys) #T) => #T)
  (check (and (memq dp0 keys) #T) => #T)
  (check (and (memq unused keys) #T) => #T))
(check (length (generate-report)) => 4)
(check (for-all profiled-procedure? (generate-report)) => #T)

(define-syntax check-pp
  (syntax-rules ()
    [(_ p sc u cs rs)
     (let ([pp (hashtable-ref (profiled-procedures-HT) p #F)])
       (let ([proc-obj (profiled-procedure-proc-obj pp)]
             [source-code (profiled-procedure-source-code pp)]
             [uses (profiled-procedure-uses pp)])
         (check proc-obj (=> eq?) p)
         (check source-code => sc)
         (check (length uses) => u)
         (check (for-all procedure-use? uses) => #T)
         (let ([starts (map procedure-use-start uses)]
               [stops (map procedure-use-stop uses)]
               [calleds (map procedure-use-called uses)]
               [returneds (map procedure-use-returned uses)])
           (check (for-all time? starts) => #T)
           (check (for-all time? stops) => #T)
           (check calleds => cs)
           (check returneds => rs))))]))

(check-pp clp0 '(case-lambda
                  [(x) (values (+ x x) (- x))]
                  [(x y) (* x y)])
          2 '(2 1) '(1 2))
(check-pp lp0 '(lambda a 
                 (apply values (reverse a)))
          3 '(1 2 4) '(1 2 4))
(check-pp dp0 '(define (dp0 x y . a)
                 (if (number? x)
                   (apply + x y a)
                   (begin ((call/cc (lambda (cc) (set! dp0-k cc) values)) 'C)
                          'R)))
          3 '(#F 2 5) '(#F 1 1))
(check-pp unused '(define (unused . _) (assert #F))
          0 '() '())

(define report-str
  (call-with-string-output-port
    (lambda (sop) (print-report #T (generate-report) sop))))
(display report-str)

(display "\n(reset-recorded-uses) (print-report)\n")
(display "No procedure uses should be printed.\n")
(reset-recorded-uses)
(print-report)


(check-report)
