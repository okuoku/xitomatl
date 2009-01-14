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
  (only (rnrs eval) environment)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (only (xitomatl strings) string-end=?)
  (xitomatl repl))

(define (string-repl in-str env)
  (let ([in (open-string-input-port in-str)])
    (let-values ([(out out-get) (open-string-output-port)]
                 [(err err-get) (open-string-output-port)])
      (repl in out err env)
      (values (out-get) (err-get)))))

(define env (environment '(rnrs)))

(define (_check-repl in out err)
  (let-values ([(o e) (string-repl in env)])
    (cond [(and (string? out) (string? err))
           (check (list o e) => (list out err))]
          [(and (string? out) (procedure? err))
           (check (list o (err e)) => (list out #T))]          
          [(and (procedure? out) (string? err))
           (check (list (out o) e) => (list #T err))]                    
          [(and (procedure? out) (procedure? err))
           (check (list (out o) (err e)) => '(#T #T))]
          [else (assert #F)])))

(define-syntax check-repl
  (lambda (stx)
    (syntax-case stx (=>)
      [(_ in => out err)
       #'(_check-repl in out err)])))

(check-repl "(+ 1 2)"
            => "> 3\n> \n" "")
(check-repl "(begin (write 'zab)(display \"foo\" (current-error-port))(values))"
            => "> zab> \n" "foo")
(check-repl "(read (current-input-port)) foo(read (current-input-port))bar "
            => "> foo\n> bar\n> \n" "")
(check-repl "(begin (raise-continuable (make-warning)) 'continued)" 
            => "> continued\n> \n" (lambda (es) (positive? (string-length es))))
(check-repl "'foo -oops 'bar"
            => "> foo\n> \n" (lambda (es) (string-end=? es "\nQuiting REPL.\n")))
(check 
 (catch ex ([(assertion-violation? ex)])
   (_check-repl "(begin (close-port (current-error-port)) (raise 'try-to-print))" "" ""))
 => #T)


(check-report)
