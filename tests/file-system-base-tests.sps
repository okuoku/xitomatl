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
  (except (rnrs) file-exists? delete-file)
  (srfi :78 lightweight-testing)
  (xitomatl file-system base)
  (xitomatl file-system paths)
  (only (xitomatl enumerators) fold/enumerator)
  (only (srfi :1 lists) list-index)
  (only (xitomatl predicates) exact-non-negative-integer?)
  (only (xitomatl exceptions) reraise))

(define-syntax check-io-f-error
  (syntax-rules ()
    [(_ who fn expr)
     (check (guard (ex [else (and (i/o-filename-error? ex)
                                  (who-condition? ex)
                                  (list (condition-who ex)
                                        (cleanse-path (i/o-error-filename ex))))])
              expr
              'unexpected-return)
            => '(who fn))]))

(define (make-test-tree)
  (define tree 
    '("xitomatl-tests" z
      ("a" x
       ("ab" x y z
        ("aba")
        ("abb" x y)))
      ("b" y z)
      ("c")
      ("d"
       ("da" y
        ("daa" z
         ("daaa" x y
          ("daaaa")
          ("daaab" z))
         ("daab"))
        ("dab" z x
         ("daba" y))))
      ("e")))
  (let make ([t tree] [p "/tmp"])
    (let ([d (path-join p (car t))])
      (make-directory d)
      (let-values ([(files dirs) (partition symbol? (cdr t))])
        (for-each (lambda (x) 
                    (call-with-output-file (path-join d (symbol->string x))
                      (lambda (fop) (write (list x x) fop))))
                  files)
        (for-each (lambda (x) (make x d))
                  dirs)))))

;; current-directory 
(current-directory "/tmp")
(check (cleanse-path (current-directory)) => "/tmp")
(make-test-tree)
(current-directory "/tmp/xitomatl-tests")
(check (cleanse-path (current-directory)) => "/tmp/xitomatl-tests")
;; directory-list
(check (list-sort string<? (directory-list ".")) 
       => '("a" "b" "c" "d" "e" "z"))
(check-io-f-error directory-list "does-not-exist"
  (directory-list "does-not-exist"))
(check-io-f-error directory-list "z"
  (directory-list "z"))
;; file-exists?
(check (file-exists? "a") => #t)
(check (file-exists? "a/x") => #t)
(check (file-exists? "doesnt-exist") => #F)
;; delete-directory
(check (delete-directory "c") => #t)
(check (file-exists? "c") => #f)
(check (delete-directory "c") => #f)
(check-io-f-error delete-directory "c"
  (delete-directory "c" #t))
(delete-directory "e" #t)  ;; returns unspecified value(s)
(check (file-exists? "e") => #f)
(assert (file-regular? "z" #F))
(check (delete-directory "z") => #f)
(check-io-f-error delete-directory "z"
  (delete-directory "z" #t))
;; delete-file
(delete-file "z")  ;; returns unspecified value(s)
(check (file-exists? "z") => #f)
(check-io-f-error delete-file "z"
  (delete-file "z"))
(check-io-f-error delete-file "d"
  (delete-file "d"))
;; change-mode
(change-mode "a/ab/abb" #o500)
(check (file-exists? "a/ab/abb") => #t)
(check-io-f-error call-with-output-file "a/ab/abb/nope"
  (call-with-output-file "a/ab/abb/nope"
    (lambda (fop) (display "nope" fop))))
(check-io-f-error change-mode "doesnt-exist"
  (change-mode "doesnt-exist" 0))
;; file-mtime file-ctime
(check (exact-non-negative-integer? (file-mtime "a/ab")) => #T)
(check (exact-non-negative-integer? (file-ctime "a/ab/abb")) => #T)
(check (>= (file-mtime "a/ab") #e1e9) => #T)
(check (>= (file-ctime "a/ab/abb") #e1e9) => #T)
(check-io-f-error file-mtime "doesnt-exist"
  (file-mtime "doesnt-exist"))
(check-io-f-error file-ctime "doesnt-exist"
  (file-ctime "doesnt-exist"))
;; make-directory
(make-directory "new" #o200)  ;; returns unspecified value(s)
(check (file-exists? "new") => #t)
(check-io-f-error make-directory "new"
  (make-directory "new"))
(check-io-f-error directory-list "new"
  (directory-list "new"))
;; file-readable? file-writable? file-executable?
(check (file-readable? "a/ab/abb") => #T)
(check (file-writable? "a/ab/abb") => #F)
(check (file-executable? "a/ab/abb") => #T)
(check (file-readable? "new") => #F)
(check (file-writable? "new") => #T)
(check (file-executable? "new") => #F)
(check-io-f-error file-readable? "doesnt-exist"
  (file-readable? "doesnt-exist"))
(check-io-f-error file-writable? "doesnt-exist"
  (file-writable? "doesnt-exist"))
(check-io-f-error file-executable? "doesnt-exist"
  (file-executable? "doesnt-exist"))
;; undo chmods for the rest of the test program
(change-mode "a/ab/abb" #o755)
(check (file-writable? "a/ab/abb") => #T)
(change-mode "new" #o755)
(check (file-readable? "new") => #T)
(check (file-executable? "new") => #T)
;; make-symbolic-link and file-exists? "follow" arg
(make-symbolic-link "../a/ab/aba" "b/sym")
(check (file-exists? "b/sym") => #t)
(check (file-exists? "b/sym" #f) => #t)
(check (delete-directory "a/ab/aba") => #t)
(check (file-exists? "b/sym") => #f)
(check (file-exists? "b/sym" #f) => #t)
(change-mode "d" #o000)
(check-io-f-error make-symbolic-link "d/nope"
  (make-symbolic-link "blah" "d/nope"))
(change-mode "d" #o755)
;; file-regular? 
(check (file-regular? "d/da/y") => #t)
(check (file-regular? "d/da/y" #f) => #t)
(check (file-regular? "d/da") => #f)
(check (file-regular? "d/da" #f) => #f)
(check (file-regular? "b/sym") => #f)
(make-symbolic-link "y" "d/da/sym")
(check (file-regular? "d/da/sym") => #t)
(check (file-regular? "d/da/sym" #f) => #f)
(make-symbolic-link ".." "d/sym")
(check (file-regular? "d/sym") => #f)
(check (file-regular? "d/sym" #f) => #f)
(check (file-regular? "doesnt-exist") => #F)
;; file-directory? 
(check (file-directory? "d") => #t)
(check (file-directory? "d" #f) => #t)
(check (file-directory? "d/da/y") => #f)
(check (file-directory? "d/da/y" #f) => #f)
(check (file-directory? "d/da/sym") => #f)
(check (file-directory? "d/da/sym" #f) => #f)
(check (file-directory? "d/sym") => #t)
(check (file-directory? "d/sym" #f) => #f)
(check (file-directory? "doesnt-exist") => #F)
;; file-symbolic-link?
(check (file-symbolic-link? "d/da/sym") => #t)
(check (file-symbolic-link? "d/sym") => #t)
(check (file-symbolic-link? "b/sym") => #t)
(check (file-symbolic-link? "d/da") => #f)
(check (file-symbolic-link? "d/da/y") => #f)
(check (file-symbolic-link? ".") => #f)
(check (file-symbolic-link? "doesnt-exist") => #F)
;; deleting symbolic link
(delete-file "d/da/sym")
(check (file-symbolic-link? "d/da/sym") => #f)
(check (file-exists? "d/da/sym") => #f)
(check (file-exists? "d/da/sym" #f) => #f)
;; rename-file
(rename-file "d/da/y" "d/da/yayaya")
(check (file-exists? "d/da/y" #F) => #F)
(check (file-regular? "d/da/yayaya" #F) => #T)
(rename-file "d/da/yayaya" "d/da/y")
(check (file-exists? "d/da/yayaya" #F) => #F)
(check (file-regular? "d/da/y" #F) => #T)
(rename-file "d/da" "d/dadada")
(check (file-exists? "d/da" #F) => #F)
(check (file-directory? "d/dadada" #F) => #T)
(rename-file "d/dadada" "d/da")
(check (file-exists? "d/dadada" #F) => #F)
(check (file-directory? "d/da" #F) => #T)
(rename-file "b/sym" "b/symsym")
(check (file-exists? "b/sym" #F) => #F)
(check (file-symbolic-link? "b/symsym") => #T)
(rename-file "b/symsym" "b/sym")
(check (file-exists? "b/symsym" #F) => #F)
(check (file-symbolic-link? "b/sym") => #T)
(assert (not (file-exists? "b/sym")))
(make-symbolic-link ".." "b/sym2")
(check-io-f-error rename-file "b/sym2"
  (rename-file "b/sym2" "b/sym"))  ;; broken symlink exists
(delete-file "b/sym2")
(check-io-f-error rename-file "doesnt-exist"
  (rename-file "doesnt-exist" "bad"))
(check-io-f-error rename-file "a/ab/x"
  (rename-file "a/ab/x" "d/da" #T))  ;; file to directory
(check-io-f-error rename-file "a/ab"
  (rename-file "a/ab" "d/da/y" #T))  ;; directory to file
(call-with-output-file "temp"
  (lambda (fop) (put-string fop (call-with-input-file "a/ab/x" get-string-all))))
(check-io-f-error rename-file "temp"
  (rename-file "temp" "a/ab/x"))  ;; "a/ab/x" already exists
(rename-file "temp" "a/ab/x" #T)
(check (file-exists? "temp" #F) => #F)
(check (file-exists? "a/ab/x" #F) => #T)
;; file-size
(call-with-port (open-file-output-port "fsz")
  (lambda (fop) (put-bytevector fop #vu8(1 2 3 4 5 6 7))))
(check (file-size "fsz") => 7)
(delete-file "fsz")
(check-io-f-error file-size "fsz"
  (file-size "fsz"))
;; directory-walk-enumerator 
(check (fold/enumerator
        (directory-walk-enumerator)
        "."
        (lambda (path dirs files syms accum)
          (define (s l) (list-sort string<? l))
          (values (s dirs) (cons* path (s dirs) (s files) (s syms) accum)))
        '())
       => '("./new" () () ()
            "./d/da/dab/daba" () ("y") ()
            "./d/da/dab" ("daba") ("x" "z") ()
            "./d/da/daa/daab" () () ()
            "./d/da/daa/daaa/daaab" () ("z") ()
            "./d/da/daa/daaa/daaaa" () () ()
            "./d/da/daa/daaa" ("daaaa" "daaab") ("x" "y") ()
            "./d/da/daa" ("daaa" "daab") ("z") ()
            "./d/da" ("daa" "dab") ("y") ()
            "./d" ("da") () ("sym")
            "./b" () ("y" "z") ("sym")
            "./a/ab/abb" () ("x" "y") ()
            "./a/ab" ("abb") ("x" "y" "z") ()
            "./a" ("ab") ("x") ()
            "." ("a" "b" "d" "new") () ()))
(check (fold/enumerator
        (directory-walk-enumerator)
        "."
        (lambda (path dirs files syms i)
          (if (string=? path "./d/da/daa/daaa")
            (values #f i)
            (values (list-sort string<? dirs) (+ 1 i))))
        0)
       => 8)
(let ([r (fold/enumerator
          (directory-walk-enumerator 'bottom-up)
          "."
          (lambda (path dirs files syms accum)
            (values #t (cons path accum)))
          '())])
  (define (li x)
    (list-index (lambda (y) (string=? x y)) r))
  (check (li ".") => 0)
  (check (< (li ".") (li "./a")) => #t)
  (check (< (li ".") (li "./b")) => #t)
  (check (< (li ".") (li "./d")) => #t)
  (check (< (li ".") (li "./new")) => #t)
  (check (< (li "./a") (li "./a/ab") (li "./a/ab/abb")) => #t)
  (check (< (li "./d") (li "./d/da") (li "./d/da/daa")) => #t)
  (check (< (li "./d/da/daa") (li "./d/da/daa/daaa") (li "./d/da/daa/daaa/daaaa")) => #t)
  (check (< (li "./d/da/daa/daaa") (li "./d/da/daa/daaa/daaab")) => #t)
  (check (< (li "./d/da/daa") (li "./d/da/daa/daab")) => #t)
  (check (< (li "./d/da") (li "./d/da/dab") (li "./d/da/dab/daba")) => #t))
(change-mode "./d/da/daa/daaa" #o000)
(check 
 (guard (ex [else (and (warning? ex)
                       (not (serious-condition? ex))
                       (who-condition? ex)
                       (message-condition? ex)
                       (irritants-condition? ex)
                       (= 2 (length (condition-irritants ex)))
                       (list (condition-who ex)
                             (condition-message ex)
                             (cadr (condition-irritants ex))))])
   (fold/enumerator
    (directory-walk-enumerator)
    "."
    (lambda (p d f s) d))
   'unexpected-return)
 => '(directory-walk-enumerator 
      "Exception raised from directory walking" 
      "./d/da/daa/daaa"))
(check (let ([raised #f]) 
         (with-exception-handler
           (lambda (ex)
             (set! raised #t)
             (reraise ex))
           (lambda ()
             (fold/enumerator
              (directory-walk-enumerator)
              "."
              (lambda (p d f s) d))))
         (list 'continued raised))
       => '(continued #t))
(change-mode "./d/da/daa/daaa" #o755)
;; directory-walk -- uses directory-walk-enumerator
(let ([x 0] [y 0])
  (directory-walk
   (lambda (p d f s)
     (set! x (apply + x (map length (list d f s))))
     (set! y (+ 1 y)))
   ".")
  (check x => 32)
  (check y => 15))
(let ([x 0] [y 0])
  (directory-walk
   (lambda (p d f s)
     (set! x (apply + x (map length (list d f s))))
     (set! y (+ 1 y)))
   "."
   'bottom-up)
  (check x => 32)
  (check y => 15))
;; directory-walk/choice -- always top-down -- uses directory-walk-enumerator
(let ([x 0] [y 0])
  (directory-walk/choice
   (lambda (p d f s)
     (set! x (apply + x (map length (list d f s))))
     (set! y (+ 1 y))
     (if (>= 4 (string-length p)) d '()))
   ".")
  (check x => 18)
  (check y => 7))
;; delete-any -- uses directory-walk bottom-up
(change-mode "./a/ab" #o500)
(check-io-f-error delete-directory "./a/ab/abb"
  (delete-any "./a/ab/abb" #t))
(change-mode "./a/ab" #o700)
(check (delete-any "d/da/daa/daaa") => #t)
(check (file-exists? "d/da/daa/daaa") => #f)
(check (delete-any "d/da/daa/daaa") => #f)
(check-io-f-error delete-file "d/da/daa/daaa"
  (delete-any "d/da/daa/daaa" #t))
(delete-any "d/da/dab/x" #t)  ;; unspecified return value(s)
(check (file-exists? "d/da/dab/x") => #f)
(check (delete-any "d/da/dab/x") => #f)
(check-io-f-error delete-file "d/da/dab/x"
  (delete-any "d/da/dab/x" #t))
(check (delete-any "b/sym") => #t)
(check (file-exists? "b/sym" #F) => #f)
(check (delete-any "b/sym") => #f)
(check-io-f-error delete-file "b/sym"
  (delete-any "b/sym" #t))

;; clean-up
(let ([tests-dir (current-directory)])
  (current-directory "/tmp")
  (delete-any tests-dir)
  (check (file-exists? tests-dir) => #f))

(check-report)
