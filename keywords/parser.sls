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
(library (xitomatl keywords parser)
  (export
    not-given not-given?
    keywords-parser--meta
    keywords-parser--define/kw)
  (import
    (rnrs)
    (for (only (rnrs base) quote) (meta -1))
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand)
    (for (only (xitomatl indexes) enumerate) expand)
    (only (xitomatl keywords other) missing-value--default
                                    missing-keyword--default predicate-false--default)
    (for (only (xitomatl keywords other) process-options) expand))

  (define not-given (list #T)) ;; unique object
  (define (not-given? x) (eq? x not-given))
  
  (define-syntax keywords-parser--meta
    (lambda (stx)
      (define (gen-stx rt-who kw=? missing-keyword predicate-false
                       kw-values process-input-list car-id cdr-id additional)
        (lambda (kw-spec index)
          (with-syntax ([(kw-id . _) kw-spec]
                        [v (gen-temp)])
            (define (gen-clause/val)
              #`((and (#,kw=? #,car-id 'kw-id)
                      (pair? #,cdr-id))
                 (vector-set! #,kw-values #,index (car #,cdr-id))
                 (#,process-input-list (cdr #,cdr-id) #,additional)))
            (define (gen-test/val true false)
              #`(let ((v (vector-ref #,kw-values #,index)))
                  (if (not-given? v) #,false #,true)))
            (define (gen-pred/val pred)
              #`(if (#,pred v)
                  v
                  (#,predicate-false '#,rt-who 'kw-id '#,pred v)))
            (define (gen-missing)
              #`(#,missing-keyword '#,rt-who 'kw-id))
            (define (gen-clause/bool)
              #`((#,kw=? #,car-id 'kw-id)
                 (vector-set! #,kw-values #,index #T)
                 (#,process-input-list #,cdr-id #,additional)))
            (define (gen-test/bool)
              #`(not (not-given? (vector-ref #,kw-values #,index))))
            (let-values ([(default predicate boolean) (process-options stx kw-spec)])
              (cond [(and default predicate)
                     (list (gen-clause/val)
                           (gen-test/val (gen-pred/val predicate) default))]
                    [default
                     (list (gen-clause/val)
                           (gen-test/val #'v default))]
                    [predicate
                     (list (gen-clause/val)
                           (gen-test/val (gen-pred/val predicate) (gen-missing)))]
                    [boolean
                     (list (gen-clause/bool)
                           (gen-test/bool))]
                    [else
                     (list (gen-clause/val)
                           (gen-test/val #'v (gen-missing)))])))))
      (syntax-case stx ()
        [(_ rt-who kw=? missing-value missing-keyword predicate-false
            [kw-id options ...] ...)
         (for-all identifier? 
                  #'(kw=? missing-value missing-keyword predicate-false kw-id ...))
         (with-syntax* ([num (length #'(kw-id ...))]
                        [(kw-values process-input-list car-id cdr-id additional)
                         (generate-temporaries '(1 2 3 4 5))]
                        [((cond-clause value-expr) ...)
                         (map (gen-stx #'rt-who #'kw=?
                                       #'missing-keyword #'predicate-false
                                       #'kw-values #'process-input-list
                                       #'car-id #'cdr-id #'additional) 
                              #'([kw-id options ...] ...)
                              (enumerate #'(kw-id ...)))])
           #'(lambda (input-list)
               (let ((kw-values (make-vector num not-given)))
                 (let process-input-list ((l input-list) (additional '()))
                   (cond ((pair? l)
                          (let ((car-id (car l)) (cdr-id (cdr l)))
                            (cond cond-clause ...
                                  (else (if (exists (lambda (x) (kw=? car-id x))
                                                    '(kw-id ...))
                                          (missing-value 'rt-who car-id)
                                          (process-input-list cdr-id
                                           (cons car-id additional)))))))
                         ((null? l)
                          (letrec* ((kw-id value-expr) ...)
                            (values kw-id ... (reverse additional))))
                         (else
                          (assertion-violation 'rt-who "not a proper list"
                                               input-list)))))))])))
  
  (define (missing-value--define/kw who kw-id)
    (missing-value--default who (syntax-case kw-id (quote)
                                  [(quote id) (syntax->datum #'id)])))
    
  (define (kw-stx=? x y)
    (syntax-case x (quote)
      [(quote id) (identifier? #'id)
       (eq? (syntax->datum #'id) y)]
      [_ #F]))
  
  (define-syntax keywords-parser--define/kw
    (syntax-rules ()
      [(_ rt-who . r)
       (keywords-parser--meta rt-who kw-stx=?
        missing-value--define/kw missing-keyword--default predicate-false--default
        . r)]))
)
