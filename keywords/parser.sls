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
    (only (xitomatl match) match)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand)
    (only (xitomatl keywords other) missing-value--default
                                    missing-keyword--default predicate-false--default)
    (for (only (xitomatl keywords other) process-options) expand))

  (define not-given (list #T)) ;; unique object
  (define (not-given? x) (eq? x not-given))
  
  (define-syntax keywords-parser--meta
    (lambda (stx)
      (define (gen-stx rt-who process-input-list kw=? missing-keyword predicate-false)
        (lambda (kw-spec kw-value)
          (with-syntax ([(kw-id . _) kw-spec])
            (let-values ([(default predicate boolean) (process-options stx kw-spec)])
              (cond [(and default predicate)
                     (list #`[(x v . r)
                              (#,kw=? x 'kw-id)
                              (begin (set! #,kw-value v)
                                     (#,process-input-list r))]
                           #`(if (not-given? #,kw-value)
                               #,default
                               (if (#,predicate #,kw-value)
                                 #,kw-value
                                 (#,predicate-false '#,rt-who 'kw-id
                                                    '#,predicate #,kw-value))))]
                    [default
                     (list #`[(x v . r)
                              (#,kw=? x 'kw-id)
                              (begin (set! #,kw-value v)
                                     (#,process-input-list r))]
                           #`(if (not-given? #,kw-value)
                               #,default
                               #,kw-value))]
                    [predicate
                     (list #`[(x v . r)
                              (#,kw=? x 'kw-id)
                              (begin (set! #,kw-value v)
                                     (#,process-input-list r))]
                           #`(if (not-given? #,kw-value)
                               (#,missing-keyword '#,rt-who 'kw-id)
                               (if (#,predicate #,kw-value)
                                 #,kw-value
                                 (#,predicate-false '#,rt-who 'kw-id
                                                    '#,predicate #,kw-value))))]
                    [boolean
                     (list #`[(x . r)
                              (#,kw=? x 'kw-id)
                              (begin (set! #,kw-value #T)
                                     (#,process-input-list r))]
                           #`(not (not-given? #,kw-value)))]
                    [else
                     (list #`[(x v . r)
                              (#,kw=? x 'kw-id)
                              (begin (set! #,kw-value v)
                                     (#,process-input-list r))]
                           #`(if (not-given? #,kw-value)
                               (#,missing-keyword '#,rt-who 'kw-id)
                               #,kw-value))])))))
      (syntax-case stx ()
        [(_ rt-who kw=? missing-value missing-keyword predicate-false
            [kw-id options ...] ...)
         (for-all identifier? 
                  #'(kw=? missing-value missing-keyword predicate-false kw-id ...))
         (with-syntax* ([(kw-value ...) (generate-temporaries #'(kw-id ...))]
                        [process-input-list (gen-temp)]
                        [((match-clause value-expr) ...)
                         (map (gen-stx #'rt-who #'process-input-list #'kw=?
                               #'missing-keyword #'predicate-false) 
                              #'([kw-id options ...] ...)
                              #'(kw-value ...))])
           #'(lambda (input-list)
               (let ([kw-value not-given] 
                     ...
                     [additional '()])               
                 (let process-input-list ([l input-list])
                   (match l
                     match-clause
                     ...
                     [(x . rest)
                      (if (exists (lambda (y) (kw=? x y)) '(kw-id ...))
                        (missing-value 'rt-who x)
                        (begin (set! additional (cons x additional))
                               (process-input-list rest)))]
                     [() 
                      (set! additional (reverse additional))]
                     [_ 
                      (assertion-violation 'rt-who "not a proper list" input-list)]))
                 (letrec* ([kw-id value-expr] 
                           ...)
                   (values kw-id ... additional)))))])))
  
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
