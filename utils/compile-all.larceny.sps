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

(import
  (rnrs)
  (only (xitomatl common) printf)
  (only (xitomatl file-system base)
        directory-walk-enumerator make-path-to rename-file)
  (xitomatl file-system paths)
  (only (xitomatl enumerators) fold/enumerator)
  (xitomatl match))

(define (move-out base temp)
  (define moves
    (fold/enumerator
     (directory-walk-enumerator)
     base
     (lambda (path dirs files syms accum)
       (let loop
           ((others-specific
             (filter
              (matches? (:regex '(seq (+ any) "." (submatch (+ alpha)) ".sls")
                                (:not "larceny")))
              (append files syms)))
            (accum accum))
         (if (null? others-specific)
           (values dirs accum)
           (let* ((orig (path-join path (car others-specific)))
                  (reloc (path-join temp orig)))
             (loop (cdr others-specific)
                   (cons (cons orig reloc) accum))))))
     '()))
  (display "Temporarily relocating other implementations' specific files ...\n")
  (for-each
   (lambda (m)
     (let ((from (car m)) (to (cdr m)))
       (printf "~a\n--> ~a\n" from to)
       (make-path-to to)
       (rename-file from to)))
   moves)
  moves)

(define (move-back moves)
  (display "Moving relocated files back ...\n")
  (for-each
   (lambda (m)
     (let ((to (car m)) (from (cdr m)))
       (printf "~a\n--> ~a\n" from to)
       (rename-file from to)))
   moves))

(define (compile)
  ;; TODO: Use Larceny facilities for compiling.
  (display "\nNow, manually make Larceny compile the library files left in the tree.\n")
  (display "When done, type ENTER to move relocated files back.\n\n")
  (get-line (current-input-port)))

(define main
  (match-lambda*
    ((base temp)
     (and (path? base) (path? temp))
     (let ((moves (move-out base temp)))
       (compile)
       (move-back moves)))
    (args
     (apply assertion-violation (car (command-line))
            "invalid command-line arguments" args))))

(apply main (cdr (command-line)))
