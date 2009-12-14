#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl file-system paths)
  (export
    path-style
    path? absolute-path? relative-path? path=?
    path-join path-split normalize-path)
  (import
    (rnrs)
    (only (srfi :39 parameters) make-parameter)
    (only (xitomatl define) define/AV)
    (xitomatl feature-cond)
    (only (xitomatl strings) string-intersperse string-split))

  (define/AV path-style
    (make-parameter (feature-cond (posix 'posix) (windows 'windows))
                    (lambda (x)
                      (if (and (symbol? x)
                               (memq x '(posix windows windows-/)))
                        x
                        (AV "not a symbol, or unknown" x)))))

  (define (sep) (case (path-style) ((posix windows-/) "/") ((windows) "\\")))

  (define (split-drive p)
    (let ((len (string-length p)))
      (let loop ((i 0))
        (cond ((= len i) (values #F (and (< 0 len) p)))
              ((and (< 0 i) (char=? #\: (string-ref p i)))
               (let ((i (+ 1 i)))
                 (values (substring p 0 i)
                         (and (< i len) (substring p i len)))))
              (else (loop (+ 1 i)))))))
  (define (drive p) (let-values (((d r) (split-drive p))) d))
  (define (no-drive p) (let-values (((d r) (split-drive p))) r))

  (define (rooted-path? p)
    (case (path-style)
      ((posix) (char=? #\/ (string-ref p 0)))
      ((windows) (char=? #\\ (string-ref (no-drive p) 0)))
      ((windows-/) (char=? #\/ (string-ref (no-drive p) 0)))))

  (define (path? x)
    (and (string? x)
         (case (path-style)
           ((posix) (positive? (string-length x)))
           ((windows windows-/) (and (no-drive x) #T)))))

  (define (absolute-path? x) (and (path? x) (rooted-path? x)))

  (define (relative-path? x) (and (path? x) (not (rooted-path? x))))

  (define (path=? x y . r) (apply string=? (map normalize-path (cons* x y r))))

  (define (normalize-path p) (apply path-join (path-split p)))

  (define (path-join . components)
    (define (split x) (string-split x s #F))
    (define s (sep))
    (let* ((c1 (filter (lambda (x) (positive? (string-length x))) components))
           (c2 (apply append (map split c1)))
           (c3 (if (and (pair? c1) (absolute-path? (car c1)))
                 (case (path-style)
                   ((posix) (if (pair? c2) (cons "" c2) (list "" "")))
                   ((windows windows-/)
                    (if (drive (car c1))
                      (if (pair? (cdr c2)) c2 (list (car c2) ""))
                      (if (pair? c2) (cons "" c2) (list "" "")))))
                 c2)))
      (string-intersperse c3 s)))

  (define (path-split p)
    (let* ((s (sep))
           (c (string-split p s #F)))
      (case (path-style)
        ((posix)
         (if (absolute-path? p) (cons s c) c))
        ((windows windows-/)
         (if (absolute-path? p)
           (if (and (pair? c) (drive (car c)))
             (cons (string-append (car c) s) (cdr c))
             (cons s c))
           c)))))

)
