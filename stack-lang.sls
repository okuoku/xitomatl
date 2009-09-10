#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl stack-lang)
  (export
    ;; From (xitomatl stack-lang core)
    S Q λS
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

  ;; Some of these λS procedures could be optimized more.

  (define current-stack (λS ds (x) (ds)))
  (define print (λS (x) () (pretty-print x)))
  (define show (λS (x) (r) (pretty-print x) x))

  (define not (λS (x) (r) (scheme:not x)))
  (define if (λS (v t f) () (scheme:if v (t) (f))))
  (define when (λS (v p) () (scheme:if v (p) (values))))
  (define unless (λS (v p) () (scheme:if v (values) (p))))

  (define list
    (λS (size . ds) (r)
      (define who (quote list))
      (scheme:unless (non-negative-integer? size)
        (AV who "not a non-negative integer" size))
      (let loop ((s (ds)) (n size) (l (quote ())))
        (scheme:if (positive? n)
          (scheme:if (null? s)
            (AV who "not enough values on data stack" size)
            (loop (cdr s) (- n 1) (cons (car s) l)))
          (begin (ds s)
                 l)))))
  (define length
    (λS (l) (r) (scheme:length l)))

  (define map
    (λS (l p) (r)
      (scheme:map (lambda (x) (push x) (p) (pop)) l)))
  (define filter
    (λS (l p) (r)
      (scheme:filter (lambda (x) (push x) (p) (pop)) l)))

  (define compose
    (λS (g f) (r)
      (Q g f)))
  (define curry
    (λS (x p) (r)
      (lambda () (push x) (p))))
)
