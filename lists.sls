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
(library (xitomatl lists)
  (export
    make-list last-pair ;; from compat
    sublist
    map/left-right/preserving map/filter
    remove-dups remv-dups remq-dups
    intersperse)
  (import
    (rnrs)
    (only (xitomatl define) define/? define/AV define/?/AV)
    (only (xitomatl predicates) exact-non-negative-integer?)
    (xitomatl lists compat))

  (define/?/AV sublist
    (case-lambda/?
      ((l start)
       (sublist l start #F))
      ((l (start exact-non-negative-integer?)
          (end (lambda (x) (or (exact-non-negative-integer? x) (not x)))))
       (unless (or (not end) (<= start end))
         (AV "invalid range" start end)) 
       (let loop ((x l) (i 0) (a '()))
         (cond ((and end (= i end))
                (reverse a))
               ((pair? x)
                (loop (cdr x) (+ 1 i) (if (>= i start) (cons (car x) a) a)))
               ((null? x)
                (cond ((> start i)
                       (AV "start index greater than list's length" start i))
                      (end
                       (AV "end index greater than list's length" end i))
                      (else
                       (reverse a))))
               (else
                (AV "not a proper list" l)))))))
  
  ; Deterministic, left-to-right map
  ; It preserves sharing as much as possible: that is, if given the pair
  ; (h . t), (and (eq? h (f h)) 
  ;;              (eq? t (map/left-right/preserving f t))) 
  ;; holds, then
  ; (eq? (map/left-right/preserving f l) l) holds as well.
  (define/?/AV (map/left-right/preserving [f procedure?] l)
    ;; TODO: could this be made tail-recursive / constant space?
    (let loop ([f f] [l l] [orig l])
      (cond [(pair? l) (let ([h (car l)] [t (cdr l)])
                         (let ([h1 (f h)] [t1 (loop f t orig)])
                           (if (and (eq? h1 h) (eq? t1 t)) 
                             l
                             (cons h1 t1))))] 
            [(null? l) '()]
            [else (AV "not a proper list" orig)])))
  
  (define/?/AV map/filter 
    ;;; map/filter is significantly more effecient than
    ;;; the equivalent (filter values (map f l))
    (case-lambda/?
      [([f procedure?] l)
       (let loop ([l l] [r '()] [orig l])
         (cond [(pair? l) (let ([x (f (car l))])
                            (loop (cdr l) (if x (cons x r) r) orig))]
               [(null? l) (reverse r)]
               [else (AV "not a proper list" orig)]))]
      [([f procedure?] l . ls)
       (let loop ([ls (cons l ls)] [r '()] [orig (cons l ls)])
         (cond [(for-all pair? ls) (let ([x (apply f (map car ls))])
                                     (loop (map cdr ls) (if x (cons x r) r) orig))]
               [(for-all null? ls) (reverse r)]
               [else (for-each (lambda (l o) (unless (or (pair? l) (null? l))
                                               (AV "not a proper list" o))) 
                               ls orig)
                     (for-each (lambda (l) (when (null? l)
                                             (AV "length mismatch" orig)))
                               ls)]))]))

  (define-syntax define-rem-dups
    (syntax-rules ()
      [(_ name rf)
       (define/AV (name l)    
         (let loop ([l l] [r '()])
           (cond [(pair? l) (let ([h (car l)] [t (cdr l)])
                              (loop (rf h t) (cons h r)))]
                 [(null? l) (reverse r)]
                 [else (AV "not a proper list" l)])))]))  
  
  (define-rem-dups remove-dups remove)
  (define-rem-dups remv-dups remv)
  (define-rem-dups remq-dups remq)
  
  (define/AV (intersperse l sep)
    (let loop ([l l] [r '()] [sep sep] [orig l])
      (cond [(pair? l) (loop (cdr l) (cons* sep (car l) r) sep orig)]
            [(null? l) (if (null? r) '() (reverse (cdr r)))]
            [else (AV "not a proper list" orig)])))
  
  #;(define/? (flatten [l list?])
    ;;; not sure exactly what I want the semantics to be
    (let loop ([l l])
      (if (pair? l)
        (let ([x (car l)] [r (cdr l)])
          (if (list? x)
            (append (loop x) (loop r))
            (cons (loop x) (loop r))))
        l)))

)
