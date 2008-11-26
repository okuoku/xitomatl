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
(library (xitomatl keywords)
  (export
    keywords-parser
    case-lambda/kw lambda/kw define/kw
    keyword-condition? condition-keyword)
  (import
    (rnrs)
    (only (xitomatl exceptions) reraise)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand)
    (only (xitomatl keywords parser) keywords-parser--meta not-given not-given?)
    (for (only (xitomatl keywords parser) keywords-parser--define/kw) expand)
    (only (xitomatl keywords other) keyword-condition? condition-keyword
          missing-value--default missing-keyword--default predicate-false--default)
    (for (only (xitomatl keywords other) parse-kw-formals process-options) expand))
  
  (define-syntax keywords-parser
    (syntax-rules ()
      [(_ . r)
       (keywords-parser/who "a keywords parser" . r)]))
  
  (define-syntax keywords-parser/who
    (syntax-rules ()
      [(_ rt-who . r)
       (keywords-parser--meta rt-who eq?
        missing-value--default missing-keyword--default predicate-false--default
        . r)]))

  (define-syntax case-lambda/kw--meta
    (lambda (stx)
      (syntax-case stx ()
        [(_ rt-who [kw-formals . body])
         (with-syntax ([((pos-id ...) ([kw-id . opts] ...) additional-id)
                        (parse-kw-formals stx #'kw-formals (gen-temp))])
           #'(lambda (pos-id ... . keywords)
               (let ([parser (keywords-parser/who rt-who [kw-id . opts] ...)])
                 (let-values ([(kw-id ... additional-id) (parser keywords)])
                   . body))))]
        [(_ rt-who [kw-formals . body] ...)
         (with-syntax ([(parser ...) (generate-temporaries #'(kw-formals ...))]
                       [(((pos-id ...) ([kw-id . opts] ...) additional-id) ...)
                        (map (lambda (kwf) (parse-kw-formals stx kwf (gen-temp)))
                             #'(kw-formals ...))])
           #'(let ([procs (list (cons (length '(pos-id ...))
                                      (lambda (pos-id ... . keywords)
                                        (let ([parser (keywords-parser/clause-failed
                                                       [kw-id . opts] ...)])
                                          (let-values ([(kw-id ... additional-id)
                                                        (parser keywords)])
                                            . body))))
                                ...)])
               (lambda args
                 (let ([len (length args)])
                   (let try-next ([procs procs])
                     (if (pair? procs)
                       (if (>= len (caar procs))
                         ((call/cc
                           (lambda (k)
                             (with-exception-handler
                               (lambda (ex)
                                 (if (clause-failed? ex)
                                   (k (lambda () (try-next (cdr procs))))
                                   (reraise ex)))
                               (lambda ()
                                 (let-values ([vals (apply (cdar procs) args)])
                                   (lambda () (apply values vals))))))))
                         (try-next (cdr procs)))
                       (apply assertion-violation 'rt-who
                              "no clause matches arguments" args)))))))])))
    
  (define-syntax keywords-parser/clause-failed
    (syntax-rules ()
      [(_ . r)
       (keywords-parser--meta ignored eq?
        missing-value/clause-failed 
        missing-keyword/clause-failed
        predicate-false/clause-failed
        . r)]))

  (define clause-failed (list #T))  ;; unique object
  (define (clause-failed? x) (eq? x clause-failed))

  (define (missing-value/clause-failed who kw-id)
    (raise clause-failed))

  (define (missing-keyword/clause-failed who kw-id)
    (raise clause-failed))

  (define (predicate-false/clause-failed who kw-id pred-form value)
    (raise clause-failed))
  
  (define-syntax case-lambda/kw
    (syntax-rules ()
      [(_ [kw-formals body0 body ...] ...)
       (case-lambda/kw--meta "a case-lambda/kw procedure"
                             [kw-formals body0 body ...] ...)]))
  
  (define-syntax lambda/kw
    (syntax-rules ()
      [(_ kw-formals body0 body ...)
       (case-lambda/kw--meta "a lambda/kw procedure"
                             [kw-formals body0 body ...])]))
    
  (define-syntax define/kw
    ;;; Optimized to process keywords at expand time.
    (lambda (stx)
      (define (gen-stx rt-who)
        (lambda (kw-spec kw-value)
          (with-syntax ([(kw-id . _) kw-spec])
            (let-values ([(default predicate boolean) (process-options stx kw-spec)])
              (cond [(and default predicate)
                     (list #`(if (not-given? #,kw-value)
                               #,default
                               (if (#,predicate #,kw-value)
                                 #,kw-value
                                 (predicate-false--default '#,rt-who 'kw-id
                                                           '#,predicate #,kw-value)))
                           #'(:default #'not-given))]
                    [default 
                     (list #`(if (not-given? #,kw-value)
                               #,default
                               #,kw-value)
                           #'(:default #'not-given))]
                    [predicate 
                     (list #`(if (#,predicate #,kw-value)
                               #,kw-value
                               (predicate-false--default '#,rt-who 'kw-id
                                                         '#,predicate #,kw-value))
                           '())]
                    [boolean 
                     (list kw-value #'(:boolean))]
                    [else 
                     (list kw-value '())])))))
      (syntax-case stx ()
        [(_ (name . kw-formals) . body)
         (with-syntax* ([((pos-id ...) ([kw-id . kw-opts] ...) additional-id ...)
                         (parse-kw-formals stx #'kw-formals)]
                        [(kw-value ...) (generate-temporaries #'(kw-id ...))]
                        [((value-expr kw-stx-opts) ...)
                         (map (gen-stx #'name) 
                              #'([kw-id . kw-opts] ...)
                              #'(kw-value ...))])
           #'(begin
               (define (proc pos-id ... kw-value ... additional-id ...)
                 (letrec* ([kw-id value-expr]
                           ...)
                   . body))
               (define first-class
                 (case-lambda/kw--meta name
                   [kw-formals
                    (proc pos-id ... kw-id ... additional-id ...)]))
               (define-syntax name
                 (lambda (stx)
                   (define parser 
                     (keywords-parser--define/kw name [kw-id . kw-stx-opts] ...))
                   (syntax-case stx ()
                     [(_ pos-id ... kw-expr (... ...))
                      ;; NOTE: Only (quote <identifier>) forms are recognized as keywords,
                      ;; unlike run-time processing which recognizes symbol values; and
                      ;; when multiple occurances of a keyword are present, only the last
                      ;; one's expression is evaluated; and additional expressions are
                      ;; evaluated only when the define/kw formals specified taking
                      ;; additionals.
                      (with-syntax* ([(kw-expr/ordered (... ...) additional-expr)
                                      (let-values ([v (parser #'(kw-expr (... ...)))]) v)]
                                     [(additional-expr (... ...))
                                      (if (positive? (length '(additional-id ...)))
                                        (list #'(list . additional-expr))
                                        '())])
                        #'(proc pos-id ... kw-expr/ordered (... ...)
                                additional-expr (... ...)))]
                     [id
                      (identifier? #'id)
                      #'first-class])))))])))
)
