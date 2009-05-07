;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
