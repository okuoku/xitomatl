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
(library (xitomatl define)
  (export 
    define-values
    define/who
    define/AV
    define/?
    define/?/AV)
  (import 
    (rnrs)
    (xitomatl define define-values)
    (for (only (xitomatl macro-utils) syntax->list) expand)
    (only (xitomatl common) format)
    (only (xitomatl exceptions) assertion-violation/conditions)
    (xitomatl conditions))
  
  (define-syntax who-wrap
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt name expr)
         (with-syntax ([who (datum->syntax #'ctxt 'who)])
           #'(let ([who 'name])
               #f  ;; prevent internal defines in expr 
               expr))])))
  
  (define-syntax define/who
    (lambda (stx)
      (syntax-case stx ()
        [(_ (fname . frmls) b0 b ...)
         (identifier? #'fname)
         #'(define/who fname
             (lambda frmls b0 b ...))]
        [(_ name expr)
         (identifier? #'name)
         #'(define name
             (who-wrap name name 
               expr))])))
  
  
  (define (make-AV who)
    (lambda (msg . irrts) 
      (apply assertion-violation who msg irrts)))
  
  (define-syntax AV-wrap
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt name expr)
         (with-syntax ([AV (datum->syntax #'ctxt 'AV)])
           #'(let ([AV (make-AV 'name)])
               #f  ;; prevent internal defines in expr 
               expr))])))
  
  (define-syntax define/AV
    (lambda (stx)
      (syntax-case stx ()
        [(_ (fname . frmls) b0 b ...)
         (identifier? #'fname)
         #'(define/AV fname
             (lambda frmls b0 b ...))]
        [(_ name expr)
         (identifier? #'name)
         #'(define name
             (AV-wrap name name 
               expr))])))
  
  (define (make-arg-check-failed who)
    (lambda (pred-form arg-name arg-value)
      (assertion-violation/conditions who "argument check failed" (list arg-value)
        (make-argument-name-condition arg-name) 
        (make-predicate-condition pred-form))))
  
  (define-syntax case-lambda/?--meta
    (lambda (stx)
      (define (frml-id frml)
        (syntax-case frml () [(id pred) #'id] [_ frml]))
      (define (needs-check? frml)
        (syntax-case frml () [(id pred) #t] [_ #f]))
      (syntax-case stx ()
        [(_ fname [frmls . body] ...)
         (with-syntax ([((f ... fr) ...) 
                        (map (lambda (f)
                               (syntax-case f ()
                                 [(f ... . #(r p)) #'(f ... (r p))]
                                 [(f ... . r) #'(f ... r)]))
                             #'(frmls ...))])
           (with-syntax ([((id ... idr) ...)
                          (map (lambda (fl) (map frml-id (syntax->list fl)))
                               #'((f ... fr) ...))]
                         [(((cid p) ...) ...) 
                          (map (lambda (fl) (filter needs-check? (syntax->list fl))) 
                               #'((f ... fr) ...))])
             #'(let ([acf (make-arg-check-failed 'fname)])
                 (case-lambda 
                   [(id ... . idr)
                    (unless (p cid) (acf 'p 'cid cid))
                    ...
                    (let () . body)]
                   ...))))])))
  
  (define-syntax define/?
    (lambda (stx)
      (syntax-case stx ()
        [(_ (fname . frmls) body0 body* ...)
         (identifier? #'fname)
         #'(define fname
             (case-lambda/?--meta fname [frmls body0 body* ...]))]
        [(_ name expr) 
         (identifier? #'name)
         (with-syntax ([CL/? (datum->syntax #'name 'case-lambda/?)]
                       [L/? (datum->syntax #'name 'lambda/?)])
           #'(define name
               (let-syntax ([CL/? (syntax-rules ()
                                    [(_ . r) (case-lambda/?--meta name . r)])]
                            [L/? (syntax-rules ()
                                   [(_ . r) (case-lambda/?--meta name r)])])
                 expr)))])))
  
  (define-syntax define/?/AV
    (lambda (stx)
      (syntax-case stx ()
        [(_ (fname . frmls) body0 body* ...)
         (identifier? #'fname)
         #'(define fname
             (AV-wrap fname fname
               (case-lambda/?--meta fname [frmls body0 body* ...])))]
        [(_ name expr)
         (identifier? #'name)
         #'(define/? name
             (AV-wrap name name
               expr))])))
)
