#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl stack-lang core)
  (export
    Q S* S
    define-λS λS λS/who
    stack #;pop #;push
    not-enough-values)
  (import
    (rnrs)
    (srfi :39 parameters)
    (for (only (xitomatl macro-utils) formals-ok?/raise) expand)
    (xitomatl stack-lang unsafe))

#|(define block-size 4000)  ;; Must be greater than 1.
  (define max-index (- block-size 1))
  (define (new-block lower)
    (let ((b (make-vector block-size)))
      (vector-set! b 0 lower)
      b))
  (define (new-stack)
    (vector 1 (new-block #F)))
  (define-syntax stack-index
    (syntax-rules () ((_ s) ($vector-ref s 0))))
  (define-syntax stack-index-set!
    (syntax-rules () ((_ s v) ($vector-set! s 0 v))))
  (define-syntax stack-block
    (syntax-rules () ((_ s) ($vector-ref s 1))))
  (define-syntax stack-block-set!
    (syntax-rules () ((_ s v) ($vector-set! s 1 v))))
  (define-syntax stack-lower-block
    (syntax-rules () ((_ s) ($vector-ref (stack-block s) 0))))

  (define-syntax %push
    (lambda (stx)
      (syntax-case stx ()
        ((_ s v)
         (identifier? (syntax s))
         (syntax
          (let ((i (stack-index s)))
            (if ($fx= block-size i)
              (let ((b (new-block (stack-block s))))
                ($vector-set! b 1 v)
                (stack-index-set! s 2)
                (stack-block-set! s b))
              (begin ($vector-set! (stack-block s) i v)
                     (stack-index-set! ($fx+ 1 i))))))))))
  
  (define-syntax %pop
    (lambda (stx)
      (syntax-case stx ()
        ((_ s)
         (identifier? (syntax s))
         (syntax
          (let ((i ($fx- (stack-index s) 1)))
            (if ($fx= 0 i)
              (let ((b (stack-lower-block s)))
                (if b
                  (let ((x ($vector-ref b max-index)))
                    ($vector-set! b max-index #F)
                    (stack-index-set! s max-index)
                    (stack-block-set! s b)
                    x)
                  (not-enough-values)))
              (let* ((b (stack-block s))
                     (x ($vector-ref b i)))
                ($vector-set! b i #F)
                (stack-index-set! s i)
                x))))))))
|#    
  ;; The reason for using a parameter is so that it's thread-local if
  ;; multi-threading happens.
  (define stack (make-parameter (quote ()) #;(new-stack)))

#;(define (pop)
    (let* ((ds (stack))
           (x ($car ds)))
      (stack ($cdr ds))
      x))

#;(define (push x) (stack (cons x (stack))))

  (define-syntax Q
    (lambda (stx)
      (define (one-ret-val? x)
        (define (literal? x)
          (syntax-case x (quote)
            (_ (identifier? x) #F)
            ((quote . _) #T)
            ((_ . _) #F)
            (_ #T)))
        (or (literal? x)
            (syntax-case x (Q λS)
              ((Q . _) #T)
              ((λS . _) #T)
              (_ #F))))
      (syntax-case stx ()
        ((_ expr ...)
         (with-syntax ((expr^ (fold-left (lambda (a e)
                                           (cond ((identifier? e)
                                                  (list e a))
                                                 ((one-ret-val? e)
                                                  (list (syntax cons) e a))
                                                 (else
                                                  (quasisyntax ((λS () v (unsyntax e))
                                                                (unsyntax a))))))
                                         (syntax s)
                                         (syntax (expr ...)))))
           (syntax (lambda (s) expr^)))))))

  (define-syntax S*
    (syntax-rules ()
      ((_ s expr ...)
       ((Q expr ...) s))))

  (define-syntax S
    (syntax-rules ()
      ((_) (values))
      ((_ expr ...)
       (begin (stack (S* (stack) expr ...))
              (values)))))

  (define not-enough-values
    (case-lambda
      ((who . irrts)
       (apply assertion-violation who "not enough values on data stack" irrts))
      (() (not-enough-values #F))))

  (define-syntax define-λS
    (lambda (stx)
      (syntax-case stx (λS)
        ((_ name (λS . r))
         (identifier? (syntax name))
         (syntax (define name (λS/who name . r))))
        ((_ . r)
         (syntax (define . r))))))

  (define-syntax λS
    (syntax-rules ()
      ((_ in-frmls out-frmls . body)
       (λS/who "a λS" in-frmls out-frmls . body))))

  (define-syntax λS/who
    (lambda (stx)
      (define (make-pop-bind who in-frmls eval-push)
        (syntax-case in-frmls ()
          ((id ... . ds)
           (with-syntax
               (((clause ...)
                 (apply append
                        (map (lambda (x)
                               (list (quasisyntax
                                      ((unsyntax x)
                                       (if (pair? s)
                                         ($car s)
                                         (not-enough-values (quote (unsyntax who))))))
                                     (syntax (s ($cdr s)))))
                             (reverse (syntax (id ...))))))
                ((maybe-ds ...)
                 (if (identifier? (syntax ds))
                   (list (syntax (ds s)))
                   (list))))
             (quasisyntax
              (let* (clause ...
                     maybe-ds ...)
                (unsyntax eval-push)))))))
      (define (make-eval-push out-frmls body)
        (syntax-case out-frmls ()
          (#F
           (quasisyntax (let () . (unsyntax body))))
          (()
           (quasisyntax
            (begin (let () . (unsyntax body))
                   s)))
          ((id)
           (quasisyntax
            (let ((id (let () . (unsyntax body))))
              (cons id s))))
          ((id ... . r)
           (with-syntax
               (((clause ...)
                 (map (lambda (x) (quasisyntax (s (cons (unsyntax x) s))))
                      (syntax (id ...))))
                ((maybe-rest ...)
                 (if (identifier? (syntax r))
                   (list (syntax
                          (s (let loop ((r r) (s s))
                               (if (null? r)
                                 s
                                 (loop ($cdr r) (cons ($car r) s)))))))
                   (list))))
             (quasisyntax
              (call-with-values
                (lambda () . (unsyntax body))
                (lambda (id ... . r)
                  (let* (clause ...
                         maybe-rest ...)
                    s))))))))
      (syntax-case stx ()
        ((_ who in-frmls out-frmls . body)
         (and (formals-ok?/raise (syntax in-frmls) stx)
              (or (not (syntax->datum (syntax out-frmls)))
                  (formals-ok?/raise (syntax out-frmls) stx)))
         (with-syntax
             ((expr (make-pop-bind (syntax who) (syntax in-frmls)
                     (make-eval-push (syntax out-frmls) (syntax body)))))
           (syntax
            (lambda (s) expr)))))))
)
