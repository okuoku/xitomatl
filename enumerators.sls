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

;; Inspired by Oleg Kiselyov's "ideal enumerator":
;;  http://okmij.org/ftp/papers/LL3-collections-enumerators.txt
;;  http://okmij.org/ftp/Scheme/enumerators-callcc.html
;;  http://okmij.org/ftp/Streams.html
;;
;; Do not mutate a collection while it is being enumerated by any of the
;; enumerators exported from this library.  You can make your own enumerators
;; that support mutation during enumeration and they are guaranteed to work
;; with `fold' (if you add a specialization) and `fold/enumerator'.

#!r6rs
(library (xitomatl enumerators)
  (export
    fold/enumerator fold fold-specialize!
    list-enumerator procedure-enumerator
    sequence-enumerator vector-enumerator string-enumerator
    input-port-enumerator hashtable-enumerator)
  (import
    (rnrs)
    (only (xitomatl define) define/? define/AV)
    (only (xitomatl generics) define-generic/temporal))
  
  (define/? (fold/enumerator [enum procedure?] coll [proc procedure?] . seeds)
    (enum coll proc seeds))
  
  (define/? (fold coll [proc procedure?] . seeds)
    ((enumerator coll) coll proc seeds))
  
  (define/? (fold-specialize! [pred procedure?] [enum procedure?])
    (enumerator-specialize! (list pred) (lambda (_) enum)))
  
  (define-generic/temporal enumerator
    [([_ (lambda (x) (or (pair? x) (null? x)))])
     list-enumerator]
    [([_ vector?])
     vector-enumerator]
    [([_ string?])
     string-enumerator]
    [([_ procedure?])
     procedure-enumerator]    
    [([_ hashtable?])
     hashtable-enumerator])
  
  (define/AV (list-enumerator coll proc seeds)  
    ;;; fold-left which can be stopped.
    (let loop ([seeds seeds] [h coll] [t coll])
      (if (pair? h)
        (let ([a (car h)] [h (cdr h)])
          (if (pair? h) 
            (if (eq? h t)
              (AV "circular list" coll)
              (let ([b (car h)] [h (cdr h)] [t (cdr t)])
                (let-values ([(continue . a-seeds) (apply proc a seeds)])
                  (if continue
                    (let-values ([(continue . b-seeds) (apply proc b a-seeds)])
                      (if continue
                        (loop b-seeds h t)
                        (apply values b-seeds)))
                    (apply values a-seeds)))))
            (if (null? h)
              (let-values ([(_ . a-seeds) (apply proc a seeds)])
                (apply values a-seeds))
              (AV "not a proper list" coll))))
        (if (null? h)
          (apply values seeds)
          (AV "not a proper list" coll)))))
  
  (define/? (sequence-enumerator [len procedure?] [ref procedure?])
    (lambda (coll proc seeds)
      (let ([l (len coll)])
        (let loop ([i 0] [seeds seeds])
          (if (= i l)
            (apply values seeds)
            (let-values ([(continue . next-seeds)
                          (apply proc (ref coll i) seeds)])
              (if continue
                (loop (+ 1 i) next-seeds)
                (apply values next-seeds))))))))
  
  (define vector-enumerator
    (sequence-enumerator vector-length vector-ref))
  
  (define string-enumerator
    (sequence-enumerator string-length string-ref))
  
  (define (procedure-enumerator coll proc seeds)
    (let loop ([seeds seeds])
      (call-with-values
       coll
       (case-lambda 
         [(elem)
          (let-values ([(continue . next-seeds) (apply proc elem seeds)])
            (if continue
              (loop next-seeds)
              (apply values next-seeds)))]
         [()
          (apply values seeds)]))))
  
  (define/? (input-port-enumerator [reader procedure?])
    (lambda (coll proc seeds)
      ;; NOTE: does not close the port
      (let loop ([seeds seeds])
        (let ([x (reader coll)])
          (if (eof-object? x)
            (apply values seeds)
            (let-values ([(continue . next-seeds) (apply proc x seeds)])
              (if continue
                (loop next-seeds)
                (apply values next-seeds))))))))
  
  (define (hashtable-enumerator coll proc seeds)
    (let* ([keys (hashtable-keys coll)]
           [l (vector-length keys)])
      (let loop ([i 0] [seeds seeds])
        (if (= i l)
          (apply values seeds)
          (let-values ([(continue . next-seeds)
                        (apply proc 
                               (let* ([k (vector-ref keys i)]
                                      [v (hashtable-ref coll k #f)])
                                 (cons k v))
                               seeds)])
            (if continue
              (loop (+ 1 i) next-seeds)
              (apply values next-seeds)))))))
  
)
