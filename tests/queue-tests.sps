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
(import
  (rnrs)
  (xitomatl queue)
  (srfi :78 lightweight-testing))

(define q (make-empty-queue))
(check-queue q)
(check (queue-length q) => 0)

(enqueue! q 'asdf)
(check-queue q)
(check (queue-length q) => 1)

(enqueue! q "blah")
(check-queue q)
(check (queue-length q) => 2)

(enqueue! q '(3 2 1))
(check-queue q)
(check (queue-length q) => 3)

(enqueue! q '#(#\a #\b #\c #\d))        
(check-queue q)
(check (queue-length q) => 4)

(check (dequeue! q) => 'asdf)
(check-queue q)
(check (queue-length q) => 3)

(check (dequeue! q) => "blah")
(check-queue q)
(check (queue-length q) => 2)

(check (dequeue! q) => '(3 2 1))
(check-queue q)
(check (queue-length q) => 1)

(check (dequeue! q) => '#(#\a #\b #\c #\d))
(check-queue q)
(check (queue-length q) => 0)

(enqueue! q 'again)
(check-queue q)
(check (queue-length q) => 1)

(check (dequeue! q) => 'again)
(check-queue q)
(check (queue-length q) => 0)

(enqueue! q 'again-again)
(check-queue q)
(check (queue-length q) => 1)

(enqueue! q "2nd")
(check-queue q)
(check (queue-length q) => 2)

(check (dequeue! q) => 'again-again)
(check-queue q)
(check (queue-length q) => 1)

(check (dequeue! q) => "2nd")
(check-queue q)
(check (queue-length q) => 0)

(check-report)
