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
(library (xitomatl profiler meta)
  (export
    def--case-lambda/profiled def--lambda/profiled def--define/profiled
    make-make-profiled-proxy case-lambda/profiled--meta
    profiled-procedure?
    profiled-procedure-proc-obj
    profiled-procedure-source-code
    profiled-procedure-uses  ;; In reverse order: newest first
    procedure-use?
    procedure-use-start 
    procedure-use-stop 
    procedure-use-called
    procedure-use-returned
    profiled-procedures-HT
    reset-recorded-uses)
  (import
    (rnrs)
    (only (xitomatl define) define/AV)
    (only (xitomatl srfi parameters) make-parameter))
  
  ;; NOTE: Not currently thread-safe
  
  (define-syntax def--case-lambda/profiled
    (syntax-rules ()
      [(_ name make-profiled-proxy)
       (define-syntax name
         (syntax-rules ()
           [(_ [formals . body] (... ...))
            (case-lambda/profiled--meta 
             '(case-lambda [formals . body] (... ...))
             make-profiled-proxy
             [formals . body] (... ...))]))]))
  
  (define-syntax def--lambda/profiled
    (syntax-rules ()
      [(_ name make-profiled-proxy)
       (define-syntax name
         (syntax-rules ()
           [(_ formals . body)
            (case-lambda/profiled--meta 
             '(lambda formals . body)
             make-profiled-proxy
             [formals . body])]))]))
  
  (define-syntax def--define/profiled
    (syntax-rules ()
      [(_ name make-profiled-proxy)
       (define-syntax name
         (lambda (stx)
           (syntax-case stx ()
             [(_ (n . formals) . body)
              (identifier? #'n)
              #'(define n 
                  (case-lambda/profiled--meta 
                   '(define (n . formals) . body)
                   make-profiled-proxy
                   [formals . body]))]
             [(_ n expr)
              (identifier? #'n)
              #'(define n expr)])))]))
  
  (define (make-make-profiled-proxy current-info info-add info-sub)
    (lambda (proc)
      (define (profiled-proxy . args)
        (let ([enter-info-adj #f] [enter-info #f] [exit-info #f]
              [call-info-adj #f] [call-info #f] [return-info #f]
              [called (length args)] [returned #f])
          (dynamic-wind
           (lambda () 
             (set! enter-info-adj (current-info))
             (set! enter-info (current-info)))
           (lambda ()
             (call-with-values
              (lambda ()
                (set! call-info-adj (current-info))
                (set! call-info (current-info))
                (apply proc args))                         
              (lambda rv
                (set! return-info (current-info))
                (set! returned (length rv))
                (apply values rv))))
           (lambda ()
             (set! exit-info (current-info))
             (let-values ([(i adj) 
                           (if called 
                             (values call-info (info-sub call-info call-info-adj))
                             (values enter-info (info-sub enter-info enter-info-adj)))])
               (let ([start (info-add i adj)]
                     [stop (info-sub (if returned return-info exit-info) adj)])
                 (record-procedure-use profiled-proxy start stop called returned)))
             ;; Clean-up in case a continuation in proc was captured.
             (set! enter-info-adj #F)
             (set! enter-info #F) 
             (set! exit-info #F)
             (set! call-info-adj #F) 
             (set! call-info #F) 
             (set! return-info #F)
             (set! called #F)
             (set! returned #F)))))
      profiled-proxy))
  
  (define-syntax case-lambda/profiled--meta
    (syntax-rules ()
      [(_ source-code make-profiled-proxy [formals . body] ...)
       (let ([profiled-proxy
              (make-profiled-proxy (case-lambda [formals . body] ...))])
         (register-procedure profiled-proxy source-code)
         profiled-proxy)]))
    
  (define-record-type profiled-procedure
    (fields proc-obj source-code (mutable uses)))
  
  (define-record-type procedure-use
    (fields start stop called returned))
  
  (define/AV profiled-procedures-HT 
    (make-parameter (make-eq-hashtable)
                    (lambda (x) 
                      (if (and (hashtable? x)
                               (eq? eq? (hashtable-equivalence-function x)))
                        x
                        (AV "not an eq-hashtable" x)))))
  
  (define (register-procedure proc source-code)
    (hashtable-set! (profiled-procedures-HT) proc 
      (make-profiled-procedure proc source-code '())))
  
  (define (record-procedure-use proc start stop called returned)
    (let ([pp (hashtable-ref (profiled-procedures-HT) proc #f)])
      (profiled-procedure-uses-set! pp 
        (cons (make-procedure-use start stop called returned) 
              (profiled-procedure-uses pp)))))  
  
  (define (reset-recorded-uses)
    (let-values ([(keys vals) (hashtable-entries (profiled-procedures-HT))])
      (vector-for-each 
        (lambda (pp)
          (profiled-procedure-uses-set! pp '())) 
        vals)))  
)
