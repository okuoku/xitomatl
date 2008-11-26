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
(library (xitomatl keywords other)
  (export
    process-options
    parse-kw-formals
    missing-value--default
    missing-keyword--default
    predicate-false--default
    keyword-condition? 
    condition-keyword)
  (import
    (rnrs)
    (only (xitomatl macro-utils) identifier?/name=? formals-ok?/raise)
    (only (xitomatl conditions) make-predicate-condition))
  
  (define (process-options full-stx kw-spec)
    (let loop ([options (with-syntax ([(_ . opts) kw-spec]) #'opts)] 
               [default #F] [predicate #F] [boolean #F])
      (syntax-case options ()
        [(:default expr . rest)
         (and (identifier?/name=? #':default ':default)
              (not default))
         (loop #'rest #'expr predicate boolean)]
        [(:predicate expr . rest)
         (and (identifier?/name=? #':predicate ':predicate)
              (not predicate))
         (loop #'rest default #'expr boolean)]
        [(:boolean . rest)
         (and (identifier?/name=? #':boolean ':boolean)
              (not boolean))
         (loop #'rest default predicate #T)]
        [()
         (not (and boolean (or default predicate)))
         (values default predicate boolean)]
        [_
         (syntax-violation #F "invalid options for keyword" full-stx kw-spec)])))  

  (define (parse-kw-formals full-stx kw-formals . default)
    (let parse ([kwf kw-formals] [pos-ids '()])
      (syntax-case kwf ()
        [([kw-id . opts] ... . additional-id)
         (let ([pos-ids (reverse pos-ids)])
           (and (formals-ok?/raise (append pos-ids #'(kw-id ... . additional-id)) full-stx)
                (cons* pos-ids
                       #'([kw-id . opts] ...)
                       (if (identifier? #'additional-id)
                         #'(additional-id)
                         default))))]
        [(pos . r)
         (parse #'r (cons #'pos pos-ids))])))
  
  (define-condition-type &keyword &condition
    make-keyword-condition keyword-condition?
    (keyword condition-keyword))

  (define (AV who msg kw-id . more)
    (raise
     (apply condition
            (make-assertion-violation)
            (make-who-condition who)
            (make-message-condition msg)
            (make-keyword-condition kw-id)
            more)))
  
  (define (missing-value--default who kw-id)
    (AV who "keyword missing value" kw-id))

  (define (missing-keyword--default who kw-id)
    (AV who "missing required keyword" kw-id))

  (define (predicate-false--default who kw-id pred-form value)
    (AV who "keyword predicate false" kw-id 
        (make-predicate-condition pred-form)
        (make-irritants-condition (list value))))  
)
