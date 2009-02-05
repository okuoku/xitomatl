;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

#!r6rs
(import
  (rnrs)
  (rnrs eval)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (for (only (xitomatl file-system base) current-directory) expand)
  (xitomatl include))

(define-syntax check-include-error
  (syntax-rules ()
    [(_ who msg expr)
     (check (catch ex ([else (and (error? ex)
                                  (who-condition? ex)
                                  (message-condition? ex)
                                  (list (condition-who ex) 
                                        (condition-message ex)))])
              (eval 'expr (environment '(rnrs) '(xitomatl include)))
              'unexpected-return)
            => '(who msg))]))

;; include
(check (let () 
         (include "file-a")
         (+ x y))
       => 3)
(check (let () 
         (define x 1)
         (define y 2)
         (include "file-b"))
       => 3)
(check (let () 
         (include "file-a")
         (include "file-b"))
       => 3)
(let-syntax ([ae (begin (assert (file-exists? "file-c")) (lambda (_) #F))])
  (ae)
  (check-include-error include/lexical-context "error while trying to include" 
    (include "file-c")))
(check-include-error include/lexical-context "error while trying to include" 
  (include "doesnt-exist"))
;; include/lexical-context
(define-syntax s
  (lambda (stx)
    (syntax-case stx ()
      [(ctxt fn)
       #'(include/lexical-context ctxt fn)])))
(check (let () 
         (s "file-a")
         (+ x y))
       => 3)
(check (let () 
         (define x 1)
         (define y 2)
         (s "file-b"))
       => 3)
(check (let () 
         (s "file-a")
         (s "file-b"))
       => 3)
;; include/resolve
(check (let ()
         (define-syntax cd (begin (current-directory "/tmp") (lambda (_) #'(begin))))
         (cd)
         (include/resolve ("xitomatl" "tests" "include") "file-a")
         (+ x y))
       => 3)
(check (let ()
         (define-syntax cd (begin (current-directory "/tmp") (lambda (_) #'(begin))))
         (cd)
         (define x 1)
         (define y 2)
         (include/resolve ("xitomatl" "tests" "include") "file-b"))
       => 3)
(check (let ()
         (define-syntax cd (begin (current-directory "/tmp") (lambda (_) #'(begin))))
         (cd)
         (include/resolve ("xitomatl" "tests" "include") "file-a")
         (include/resolve ("xitomatl" "tests" "include") "file-b"))
       => 3)
(check-include-error include/lexical-context "error while trying to include" 
  (include/resolve ("xitomatl" "tests" "include") "file-c"))
(check-include-error include/resolve "cannot find file in search paths"
  (include/resolve ("no" "where") "doesnt-exist"))


(check-report)
