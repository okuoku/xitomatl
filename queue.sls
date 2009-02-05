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
(library (xitomatl queue)  
  (export 
    queue?
    make-empty-queue
    enqueue!
    dequeue!
    queue-length
    queue-empty?
    queue->list
    queue->list/reset
    check-queue)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (xitomatl define) define/AV))
  
  (define-record-type queue 
    (fields (mutable head) (mutable end)))
  
  (define (make-empty-queue)
    (make-queue '() '()))
  
  (define (enqueue! q e)
    (let ([el (cons e '())])
      (let ([qe (queue-end q)])
        (unless (null? qe) (set-cdr! qe el))
        (queue-end-set! q el)
        (when (null? (queue-head q)) (queue-head-set! q el)))))
  
  (define/AV (dequeue! q)
    (let ([h (queue-head q)])
      (when (null? h) (AV "empty queue"))
      (let ([rest (cdr h)])
        (queue-head-set! q rest)
        (when (null? rest) (queue-end-set! q '())))
      (car h)))
  
  (define (queue-length q)
    (length (queue-head q)))
  
  (define (queue-empty? q)
    (= 0 (queue-length q)))
  
  (define (queue->list q)
    (apply list (queue-head q)))
  
  (define (queue->list/reset q)
    (let ([h (queue-head q)])
      (queue-head-set! q '())
      (queue-end-set! q '())
      h))
  
  (define/AV (check-queue q)
    (let ([head (queue-head q)]
          [end (queue-end q)])
      (if (null? head)
        (unless (null? end)
          (AV "head is null but end is not"))
        (unless (eq? end (list-tail head (- (length head) 1)))
          (AV "last pair of head is not end")))
      (if (null? end)
        (unless (null? head)
          (AV "end is null but head is not"))
        ; We know head is not null, therefore previous if block 
        ; will have checked that last pair of head is end
        )))
)
