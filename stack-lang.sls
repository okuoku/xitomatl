#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl stack-lang)
  (export
    ;; From (xitomatl stack-lang core)
    S define-λS λS Q
    ;; From this library
    current-stack
    print
    show
    not
    if
    when
    unless
    list
    length
    map
    filter
    compose
    curry)
  (import
    (except (rename (rnrs)
                    (assertion-violation AV))
            not if when unless list length map filter)
    (prefix (only (rnrs)
                  not if when unless list length map filter)
            scheme:)
    (only (xitomatl common) pretty-print)
    (only (xitomatl predicates) non-negative-integer?)
    (xitomatl stack-lang core))

  (define (pop)
    (let* ((ds (data-stack))
           (x (car ds)))
      (data-stack (cdr ds))
      x))
  (define (push x) (data-stack (cons x (data-stack))))

  ;; Some of these λS procedures could be optimized more.

  (define-λS (current-stack . ds) (ds))
  (define-λS (print x) (pretty-print x) (values))
  (define-λS (show x) (pretty-print x) x)

  (define-λS (not x) (scheme:not x))
  (define-λS (if v t f) (scheme:if v (t) (f)))
  (define-λS (when v p) (scheme:if v (p) (values)))
  (define-λS (unless v p) (scheme:if v (values) (p)))

  (define-λS (list size . ds)
    (define who 'list)
    (scheme:unless (non-negative-integer? size)
      (AV who "not a non-negative integer" size))
    (let loop ((s (ds)) (n size) (l '()))
      (scheme:if (positive? n)
        (scheme:if (null? s)
          (AV who "not enough values on data stack" size)
          (loop (cdr s) (- n 1) (cons (car s) l)))
        (begin (ds s)
               l))))
  (define-λS (length l) (scheme:length l))

  (define-λS (map l p)
    (scheme:map (lambda (x)
                  (push x)
                  (S p)
                  (pop))
                l))
  (define-λS (filter l p)
    (scheme:filter (lambda (x)
                     (push x)
                     (S p)
                     (pop))
                   l))

  (define-λS (compose g f) (Q g f))
  (define-λS (curry x p) (lambda () (push x) (S p)))
)
