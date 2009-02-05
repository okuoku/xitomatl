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
(library (xitomatl r6rs-bindings utils)
  (export
    all-libraries-names 
    names-of)
  (import
    (rnrs)
    (rnrs eval)
    (rnrs r5rs)
    (only (xitomatl define) define/AV)
    (only (xitomatl exceptions) catch)
    (only (xitomatl library-utils) library-name<? library-name?)
    (xitomatl r6rs-bindings spec))
  
  (define all-libraries-names
    (list-sort library-name<?
               (filter library-name? (map car spec)) #| only (rnrs ---) libraries |# ))
  
  (define/AV names-of
    ;; This is a hack, and that's all I use it for.
    (case-lambda
      [(libname)
       (names-of libname 'all)]
      [(libname type)
       (let ([names (cdr (or (assoc libname spec)
                             (AV "not in spec" libname)))]
             [env (cond
                    [(library-name? libname) 
                     (environment libname)]
                    [(eq? libname 'null-environment)
                     (null-environment 5)]
                    [(eq? libname 'scheme-report-environment)
                     (scheme-report-environment 5)]
                    [else (AV "invalid library name" libname)])])
         (case type
           ;; NOTE: It's possible a binding that is an identifier-syntax will
           ;; be considered a variable and not syntax by the below, but I don't
           ;; think there will ever be any of those in the (rnrs ---) libraries.
           [(syntaxes)
            (filter (lambda (x)
                      (catch ex ([else #T])
                        (eval x env)
                        #F))
                    names)]
           [(variables)
            (filter (lambda (x)
                      (catch ex ([else #F])
                        (eval x env)
                        #T))
                    names)]
           [(procedures)
            (filter (lambda (x)
                      (catch ex ([else #F])
                        (procedure? (eval x env))))
                    names)]
           [(all) names]
           [else (AV "invalid mode" type)]))]))
)
