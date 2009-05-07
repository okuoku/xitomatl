;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

;; TODO: Don't automatically normalize paths.
;;       Rename cleanse-path to normalize-path.
;;       Rename dir-sep-str to path-separator.
;;       Remove dir-sep-char and root-dir-str.
;;       Maybe more clean-up.

#!r6rs
(library (xitomatl file-system paths)
  (export
    dir-sep-char dir-sep-str root-dir-str
    path? absolute-path? relative-path? path=? 
    path-join path-split cleanse-path)
  (import
    (rnrs)
    (srfi :0 cond-expand)
    (only (xitomatl define) define/?)
    (xitomatl strings))
  
  ;; If anyone ever wants to use my libraries on Windows, we can cond-expand or
  ;; something this library to work.
  ;; TODO: Make run-time parameter to control if POSIX or Windows style is done;
  ;;       this way, either platform can work with paths for the other.
  
  (define dir-sep-char #\/)
  (define dir-sep-str (string dir-sep-char)) ;; must be length of 1
  (define (root-dir-str) dir-sep-str)  ;; for Windows, could be parameter, to change drives
  
  (define (starts-with-root? p)
    #;(string=? (root-dir-str) (substring p 0 (string-length (root-dir-str))))
    (char=? dir-sep-char (string-ref p 0)))
  
  (define (path? x)
    (and (string? x) (positive? (string-length x))))
  
  (define (absolute-path? x)
    (and (path? x) (starts-with-root? x)))
  
  (define (relative-path? x)
    (and (path? x) (not (starts-with-root? x))))
  
  (define (path=? x y . r)
    (apply string=? (map cleanse-path (cons* x y r))))
  
  (define/? (path-join . #(ps (lambda (ps) (for-all string? ps))))
    (let* ((ps (filter (lambda (p) (positive? (string-length p))) ps))
           (r (string-intersperse (apply append (map _path-split ps))
                                  dir-sep-str)))
      (if (and (not (null? ps))
               (absolute-path? (car ps)))  ;; Windows version wouldn't do this, just return r
        (string-append dir-sep-str r)
        r)))
  
  (define (_path-split p)
    (string-split p dir-sep-str #f))
  
  (define (path-split p)
    (let ([r (_path-split p)])
      (if (absolute-path? p)  ;; Windows version wouldn't do this, just return r
        (cons dir-sep-str r)
        r)))
  
  (define (cleanse-path p)
    (apply path-join (path-split p)))
  
  ;;--------------------------------------------------------------------------

  (cond-expand 
    [posix]  ;; OK
    [else (error "(library (xitomatl file-system paths))"
                 "Only POSIX currently supported.")])
  
)
