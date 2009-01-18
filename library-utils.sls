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
(library (xitomatl library-utils)
  (export
    library-name-without-version
    library-name-version
    library-name?
    library-name-symbol?
    library-version?
    library-name<?
    library-version<?)
  (import
    (rnrs)
    (only (xitomatl define) define/?)
    (only (xitomatl predicates) list-of? exact-non-negative-integer?
                                pairwise? symbol<?))
  
  (define/? (library-name-without-version [name library-name?])
    (filter symbol? name))

  (define/? (library-name-version [name library-name?])
    (let ([last (list-ref name (- (length name) 1))])
      (and (list? last)
           last)))
 
  (define (library-name? x)
    (and (pair? x)
         (library-name-symbol? (car x))
         (let loop ([x (cdr x)])
           (if (pair? x)
             (if (library-name-symbol? (car x))
               (loop (cdr x))
               (and (library-version? (car x))
                    (null? (cdr x))))
             (null? x)))))

  (define (library-name-symbol? x)
    (and (symbol? x)
         (positive? (string-length (symbol->string x)))))

  (define library-version? (list-of? exact-non-negative-integer?))

  (define library-name<?
    (pairwise?
     (letrec ([name<?
               (lambda (x y)
                 (if (pair? x)
                   (and (pair? y)
                        (if (symbol? (car x))
                          (and (symbol? (car y))
                               (or (symbol<? (car x) (car y))
                                   (and (symbol=? (car x) (car y))
                                        (name<? (cdr x) (cdr y)))))
                          (or (symbol? (car y))
                              (library-version<? (car x) (car y)))))
                   (pair? y)))])
       name<?)
     (lambda (x)
       (if (library-name? x)
         x
         (assertion-violation 'library-name<? "not a library name" x)))))

  (define library-version<?
    (pairwise?
     (letrec ([version<?
               (lambda (x y)
                 (if (pair? x)
                   (and (pair? y)
                        (or (< (car x) (car y))
                            (and (= (car x) (car y))
                                 (version<? (cdr x) (cdr y)))))
                   (pair? y)))])
       version<?)
     (lambda (x)
       (if (library-version? x)
         x
         (assertion-violation 'library-version<?
                              "not a library version" x)))))

)
