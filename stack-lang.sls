#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl stack-lang)
  (export
    ;; From (xitomatl stack-lang core)
    S Q λS define
    ;; From this library
    current-stack
    print
    show
    dup
    drop
    swap
    (rename
     (S:not not)
     (S:if if)
     (S:when when)
     (S:unless unless)
     (S:list list)
     (S:length length)
     (S:map map)
     (S:filter filter))
    compose
    curry)
  (import
    (except (rnrs) define)
    (only (xitomatl common) pretty-print)
    (rename (xitomatl stack-lang core) (define-λS define)))

  (define current-stack (λS ds (r) ds))
  (define print (λS (x) () (pretty-print x)))
  (define show (λS (x) (r) (pretty-print x) x))

  (define (dup ds)
    (if (pair? ds)
      (cons (car ds) ds)
      (not-enough-values 'dup)))
  (define (drop ds)
    (if (pair? ds)
      (cdr ds)
      (not-enough-values 'drop)))
  (define swap (λS (x y . ds) #F (cons x (cons y ds))))

  (define S:not (λS/who not (x) (r) (not x)))
  (define S:if (λS/who if (v t f . ds) #F (if v (t ds) (f ds))))
  (define S:when (λS/who when (v t . ds) #F (if v (t ds) ds)))
  (define S:unless (λS/who unless (v f . ds) #F (if v ds (f ds))))

  (define S:list
    (λS/who list (size . ds) (r)
      (define who (quote list))
      (let loop ((s ds) (n size) (l (quote ())))
        (if (positive? n)
          (if (null? s)
            (not-enough-values who size)
            (loop (cdr s) (- n 1) (cons (car s) l)))
          l))))
  (define S:length
    (λS/who length (l) (r) (length l)))

  (define S:map
    (λS/who map (l p . ds) #F
      (let loop ((l l) (ds ds) (a (quote ())))
        (if (null? l)
          (cons (reverse a) ds)
          (let ((ds (p (cons (car l) ds))))
            (loop (cdr l) (cdr ds) (cons (car ds) a)))))))
  (define S:filter
    (λS/who filter (l p . ds) #F
      (let loop ((l l) (ds ds) (a (quote ())))
        (if (null? l)
          (cons (reverse a) ds)
          (let* ((v (car l))
                (ds (p (cons v ds))))
            (loop (cdr l) (cdr ds) (if (car ds) (cons v a) a)))))))

  (define compose (λS (g f) (r) (Q g f)))
  (define curry
    (λS (x p) (r)
      (lambda (ds) (p (cons x ds)))))
)
