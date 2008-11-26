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
(library (xitomatl macro-utils)
  (export
    gen-temp syntax->list with-syntax*
    duplicate-id unique-ids? unique-ids?/raise formals-ok?/raise
    identifier-append name=? identifier?/name=?)
  (import
    (rnrs)
    (only (xitomatl predicates) name=?))
  
  (define (gen-temp)
    (with-syntax ([(t) (generate-temporaries '(1))])
      #'t))
     
  (define (syntax->list ls)
    (syntax-case ls ()
      [(ls ...) #'(ls ...)]
      [_ (assertion-violation 'syntax->list "not a syntax list" ls)]))
  
  (define-syntax with-syntax*
    (syntax-rules ()
      [(_ (pc0 pc1 pc* ...) b b* ...)
       (with-syntax (pc0)
         (with-syntax* (pc1 pc* ...) b b* ...))]
      [(_ pc b b* ...)
       (with-syntax pc b b* ...)]))
  
  (define (duplicate-id ids)
    (unless (and (list? ids) (for-all identifier? ids))
      (assertion-violation 'duplicate-id "not a list of identifiers" ids))
    (let recur ([ls ids])
      (and (pair? ls)
           (let ([id (car ls)] [rest (cdr ls)])
             (if (memp (lambda (x) (bound-identifier=? x id)) rest)
               id
               (recur (cdr ls)))))))
  
  (define (unique-ids? ls)
    (not (duplicate-id ls)))
  
  (define unique-ids?/raise
    (case-lambda
      [(ids orig-stx msg)
       (let ([dup (duplicate-id ids)])
         (if dup
           (syntax-violation #f msg orig-stx dup)
           #t))]
      [(ids orig-stx)
       (unique-ids?/raise ids orig-stx "duplicate identifier")]))
  
  (define (formals-ok?/raise frmls-stx orig-stx)
    (syntax-case frmls-stx ()
      [(arg* ... . rest)
       (and (or (null? (syntax->datum #'rest))
                (identifier? #'rest)
                (syntax-violation #f "not an identifier" orig-stx #'rest))
            (for-all (lambda (id)
                       (or (identifier? id)
                           (syntax-violation #f "not an identifier" orig-stx id)))
                     #'(arg* ...))
            (unique-ids?/raise 
              (append
                #'(arg* ...)
                (if (identifier? #'rest) (list #'rest) '())) 
              orig-stx))]))
  
  (define (identifier-append ctxt . ids)
    (define who 'identifier-append)
    (unless (identifier? ctxt) (assertion-violation who "not an identifier" ctxt))    
    (let ([rs
           (apply string-append
             (map 
               (lambda (id)
                 (cond [(identifier? id) (symbol->string (syntax->datum id))]
                       [(symbol? id) (symbol->string id)]
                       [(string? id) id]
                       [else (assertion-violation who 
                               "not an identifier, symbol, or string" id)]))
               ids))])
      (unless (positive? (string-length rs))
        (assertion-violation who "result length zero" rs))
      (datum->syntax ctxt (string->symbol rs))))
  
  (define (identifier?/name=? id name)
    (and (identifier? id)
         (name=? id name)))
  
)
