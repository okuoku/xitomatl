#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl stack-lang core)
  (export
    S
    define-λS
    λS
    Q
    data-stack)
  (import
    (rnrs)
    (srfi :39 parameters))

  ;; The reason for using a parameter is so that it's thread-local if
  ;; multi-threading happens.
  (define data-stack (make-parameter (quote ())))

  (define-syntax S
    (lambda (stx)
      (syntax-case stx ()
        ((_ expr ...)
         (with-syntax (((expr^ ...)
                        (map (lambda (e)
                               (syntax-case e (unquote)
                                 (_ (identifier? e) e)
                                 ((unquote x) (syntax (lambda () x)))
                                 (x (syntax (lambda () x)))))
                             (syntax (expr ...)))))
           (syntax
            (eval-stack-exprs expr^ ...)))))))

  (define (eval-stack-exprs . thunks)
    (let loop ((thunks thunks))
      (unless (null? thunks)
        (call-with-values (car thunks)
         (lambda vals
           (unless (null? vals)
             (let loop ((v (car vals)) (r (cdr vals)) (ds (data-stack)))
               (if (null? r)
                 (data-stack (cons v ds))
                 (loop (car r) (cdr r) (cons v ds)))))))
        (loop (cdr thunks))))
    (values))

  (define-syntax define-λS
    (syntax-rules ()
      ((_ (name . formals) . body)
       (define name (λS/who name formals . body)))))

  (define-syntax λS
    (syntax-rules ()
      ((_ . r)
       (λS/who "a λS" . r))))

  (define (pop who n ds?)
    (let loop ((n n)
               (ds (data-stack))
               (a (if ds? (list data-stack) '())))
      (if (zero? n)
        (begin (data-stack ds)
               a)
        (if (null? ds)
          (assertion-violation who "empty data stack")
          (loop (- n 1) (cdr ds) (cons (car ds) a))))))

  (define-syntax λS/who
    (lambda (stx)
      (syntax-case stx ()
        ((_ who () . body)
         (syntax (lambda () . body)))
        ((_ who ds . body)
         (identifier? (syntax ds))
         (syntax (lambda () (let ((ds data-stack)) . body))))
        ((_ who (a ... . ds) . body)
         (with-syntax
             ((n (length (syntax (a ...))))
              (ds? (identifier? (syntax ds)))
              ((a^ ...) (append (syntax (a ...))
                                (if (identifier? (syntax ds))
                                  (list (syntax ds))
                                  '()))))
           (syntax
            (lambda ()
              (apply (lambda (a^ ...) . body)
                     (pop 'who n ds?)))))))))

  (define-syntax Q
    (syntax-rules ()
      ((_ expr ...)
       (lambda () (S expr ...)))))
)
