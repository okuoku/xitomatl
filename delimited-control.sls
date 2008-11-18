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
;; Taken from Oleg's http://okmij.org/ftp/Scheme/delim-control-n.scm
;; NOTE: Not currently designed for multi-threaded use.
;;       Won't work across phases on a multiple-instantiation system.

(library (xitomatl delimited-control)
  (export 
    abort prompt control shift reset prompt0 control0 shift0 reset0)
  (import 
    (rnrs))
  
  (define cells '())
  (define (cell-push! x) (set! cells (cons x cells)))
  (define (cell-pop!) 
    (let ([x (car cells)]) 
      (set! cells (cdr cells)) 
      x))

  (define-record-type cell (fields cont (mutable mark)))
  
  ; Essentially this is the ``return from the function''
  (define (abort-top! v) ((cell-cont (cell-pop!)) v))
  
  (define (unwind-till-marked! keep? accum)
    (let ([c (if (null? cells)
               (error 'unwind-till-marked! "no prompt set")
               (car cells))])  ; peek at the top cell
      (if (cell-mark c)	; if marked, it's prompt's cell
        (begin (unless keep? (cell-mark-set! c #f))
               accum)
        (begin (set! cells (cdr cells)) ; remove cell from the top of stack 
               (unwind-till-marked! keep? (cons c accum))))))
  
  (define (make-control shift? keep?)
    (lambda (f)
      (call/cc
        (lambda (k-control)
          (let* ([cells-prefix (unwind-till-marked! keep? '())]
                 [invoke-subcont (lambda (v)
                                   (call/cc
                                     (lambda (k-return)
                                       (cell-push! (make-cell k-return shift?))
                                       (for-each cell-push! cells-prefix)
                                       (k-control v))))])
            (abort-top! (f invoke-subcont)))))))
  
  (define (prompt* thunk)
    (call/cc
      (lambda (outer-k)
        (cell-push! (make-cell outer-k #t)) ; it's prompt's cell
        (abort-top! (thunk)))))
  
  (define control* (make-control #f #t))
  
  (define (abort v) (control* (lambda (ignore) v)))
  
  (define-syntax prompt
    (syntax-rules ()
      [(_ e) (prompt* (lambda () e))]))
  
  (define-syntax control
    (syntax-rules ()
      ((_ k e) (control* (lambda (k) e)))))
  
  (define-syntax reset
    (syntax-rules ()
      [(_ e) (prompt e)]))  
  
  (define shift* (make-control #t #t))
  
  (define-syntax shift
    (syntax-rules ()
      [(_ k e) (shift* (lambda (k) e))]))
  
  (define-syntax prompt0
    (syntax-rules ()
      [(_ e) (prompt e)]))
  
  (define control0* (make-control #f #f))
  
  (define-syntax control0
    (syntax-rules ()
      [(_ k e) (control0* (lambda (k) e))]))
  
  (define-syntax reset0
    (syntax-rules ()
      [(_ e) (prompt e)]))  
  
  (define shift0* (make-control #t #f))
  
  (define-syntax shift0
    (syntax-rules ()
      [(_ k e) (shift0* (lambda (k) e))]))
)
