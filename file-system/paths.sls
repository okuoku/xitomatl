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
(library (xitomatl file-system paths)
  (export
    dir-sep-char dir-sep-str root-dir-str
    path? absolute-path? relative-path? path=? 
    path-join path-split cleanse-path)
  (import
    (rnrs)
    (srfi :0 cond-expand)
    (xitomatl strings))
  
  ;; If anyone ever wants to use my libraries on Windows,
  ;; we can cond-expand or something this library to work.
  
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
  
  (define (path-join . ps)
    (let ([r (string-intersperse (apply append (map _path-split ps)) 
                                 dir-sep-str)])
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
