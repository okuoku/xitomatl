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
  (xitomatl srfi lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl common))

(define-syntax check-AV
  (syntax-rules ()
    [(_ expr)
     (check (catch ex ([else (assertion-violation? ex)])
              expr
              'unexpected-return)
            => #T)]))

;; add1
(check (add1 1) => 2)
(check (add1 -123) => -122)
(check (add1 (greatest-fixnum)) => (+ 1 (greatest-fixnum)))
(check-AV (add1 "oops"))
;; sub1
(check (sub1 1) => 0)
(check (sub1 -123) => -124)
(check (sub1 (least-fixnum)) => (- (least-fixnum) 1))
(check-AV (sub1 "oops"))
;; format   TODO?: more comprehensive
(check (format "~~" ) => "~")
(check (format "~s" '(a "b" #\c)) => "(a \"b\" #\\c)")
(check (format "~a" '(a "b" #\c)) => "(a b c)")
(check (format "~b" #b10101100) => "10101100")
(check (format "~o" #o7654321) => "7654321")
(check (string-upcase (format "~x" #xB)) => "B")
(check (string-upcase (format "~x" #xDEADBEEF)) => "DEADBEEF")
;; printf
(let ([fn "/tmp/xitomatl-common-tests--printf"])
  (with-output-to-file fn
    (lambda () (printf "\nTHIS ~s ~x ~a ~~ ~b ~o\n" "is" 10 'TEST 255 8)))
  (check (string-upcase (call-with-input-file fn get-string-all)) 
         => "\nTHIS \"IS\" A TEST ~ 11111111 10\n")
  (delete-file fn))
;; fprintf
(let-values ([(sop get) (open-string-output-port)])
  (fprintf sop "\nTHIS ~s ~x ~a ~~ ~b ~o\n" "is" 10 'TEST 255 8)
  (check (string-upcase (get)) => "\nTHIS \"IS\" A TEST ~ 11111111 10\n"))
;; pretty-print
(let ([fn "/tmp/xitomatl-common-tests--pretty-print"])
  (with-output-to-file fn
    (lambda () (pretty-print '(a "b" #\c))))
  (check (call-with-input-file fn read) => '(a "b" #\c))
  (delete-file fn))
(let-values ([(sop get) (open-string-output-port)])
  (pretty-print '(a "b" #\c) sop)
  (check (read (open-string-input-port (get))) => '(a "b" #\c)))
;; gensym
(let ([g (gensym)])
  (check (symbol? g) => #T)
  (check (eq? g g) => #T)
  (check (symbol=? g g) => #T)
  (check (eq? g (string->symbol (symbol->string g))) => #F)
  (check (symbol=? g (string->symbol (symbol->string g))) => #F))
;; time -- how to test...?
#;(check  => )
;; with-output-to-string
(check (with-output-to-string 
        (lambda () (display "something")))
       => "something")


(check-report)
