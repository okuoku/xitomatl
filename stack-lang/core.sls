#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl stack-lang core)
  (export
    S
    Q
    λS
    data-stack
    pop
    push)
  (import
    (rnrs)
    (srfi :39 parameters)
    (for (only (xitomatl macro-utils) formals-ok?/raise) expand))

  ;; The reason for using a parameter is so that it's thread-local if
  ;; multi-threading happens.
  (define data-stack (make-parameter (quote ())))

  (define (pop)
    (let* ((ds (data-stack))
           (x (car ds)))
      (data-stack (cdr ds))
      x))

  (define (push x) (data-stack (cons x (data-stack))))

  (define-syntax S
    ;; TODO?: Should it allow only identifiers, literals, and (Q ---) ?
    ;;        I.e., disallow arbitrary Scheme expressions?
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
         (with-syntax (((expr^ ...)
                        (map (lambda (e)
                               (cond ((identifier? e)
                                      (list e))
                                     ((one-ret-val? e)
                                      (list (syntax push) e))
                                     (else
                                      (quasisyntax ((λS () v (unsyntax e)))))))
                             (syntax (expr ...)))))
           (syntax (begin expr^ ...)))))))

  (define-syntax Q
    (syntax-rules ()
      ((_ expr ...)
       (lambda () (S expr ...)))))

  (define (not-enough-values)
    (assertion-violation #F "not enough values on data stack"))

  (define-syntax λS
    (lambda (stx)
      (define (make-expr in-frmls eval-push)
        (syntax-case in-frmls ()
          (() eval-push)
          (ds (identifier? (syntax ds))
           (quasisyntax
            (let ((ds data-stack))
              (unsyntax eval-push))))
          ((id ... . ds)
           (with-syntax
               (((clause ...)
                 (apply append
                        (map (lambda (x)
                               (list (quasisyntax
                                      ((unsyntax x)
                                       (if (pair? s)
                                         (car s)
                                         (not-enough-values))))
                                     (syntax (s (cdr s)))))
                             (reverse (syntax (id ...))))))
                ((maybe-ds ...)
                 (if (identifier? (syntax ds))
                   (list (syntax (ds data-stack)))
                   (list))))
             (quasisyntax
              (let* (maybe-ds ...
                     (s (data-stack))
                     clause ...)
                (data-stack s)
                (unsyntax eval-push)))))))
      (define (make-eval-push out-frmls body)
        (syntax-case out-frmls ()
          (()
           (quasisyntax (let () . (unsyntax body))))
          ((id)
           (quasisyntax
            (let ((id (let () . (unsyntax body))))
              (data-stack (cons id (data-stack))))))
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
                                 (loop (cdr r) (cons (car r) s)))))))
                   (list))))
             (quasisyntax
              (call-with-values
                (lambda () . (unsyntax body))
                (lambda (id ... . r)
                  (let* ((s (data-stack))
                         clause ...
                         maybe-rest ...)
                    (data-stack s)))))))))
      (syntax-case stx ()
        ((_ in-frmls out-frmls . body)
         (and (formals-ok?/raise (syntax in-frmls) stx)
              (formals-ok?/raise (syntax out-frmls) stx))
         (with-syntax
             ((expr (make-expr (syntax in-frmls)
                     (make-eval-push (syntax out-frmls) (syntax body)))))
           (syntax (lambda () expr)))))))
)
