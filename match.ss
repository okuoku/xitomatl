;; Modified by Derick Eddington to be an Ikarus R6RS library.
;; Custom structure stuff from the original implementation
;; is disabled, and R6RS/Ikarus structures are not yet supported.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Matching Syntactic Extensions for Scheme
;;
;; (define match:version "Version 1.18, July 17, 1995")
;;
;; Written by Andrew K. Wright, 1993 (wright@research.nj.nec.com).
;; Adapted from code originally written by Bruce F. Duba, 1991.
;; This package also includes a modified version of Kent Dybvig's
;; define-structure (see Dybvig, R.K., The Scheme Programming Language,
;; Prentice-Hall, NJ, 1987).
;;
;; This software is in the public domain.  Feel free to copy,
;; distribute, and modify this software as desired.  No warranties
;; nor guarantees of any kind apply.  Please return any improvements
;; or bug fixes to wright@research.nj.nec.com so that they may be included
;; in future releases.
;;
;; This macro package extends Scheme with several new expression forms.
;; Following is a brief summary of the new forms.  See the associated
;; LaTeX documentation for a full description of their functionality.
;;
;;
;;         match expressions:
;;
;; exp ::= ...
;;       | (match exp clause ...)
;;       | (match-lambda clause ...)
;;       | (match-lambda* clause ...)
;;       | (match-let ((pat exp) ...) body)
;;       | (match-let* ((pat exp) ...) body)
;;       | (match-letrec ((pat exp) ...) body)
;;       | (match-define pat exp)
;;
;; clause ::= (pat body) | (pat => exp)
;;
;;         patterns:                       matches:
;;
;; pat ::= identifier                      anything, and binds identifier
;;       | _                               anything
;;       | ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | 'sexp                           an s-expression
;;       | 'symbol                         a symbol (special case of s-expr)
;;       | (pat_1 ... pat_n)               list of n elements
;;       | (pat_1 ... pat_n . pat_{n+1})   list of n or more
;;       | (pat_1 ... pat_n pat_n+1 ooo)   list of n or more, each element
;;                                           of remainder must match pat_n+1
;;       | #(pat_1 ... pat_n)              vector of n elements
;;       | #(pat_1 ... pat_n pat_n+1 ooo)  vector of n or more, each element
;;                                           of remainder must match pat_n+1
;;       | #&pat                           box
;;       | ($ struct-name pat_1 ... pat_n) a structure
;;       | (= field pat)                   a field of a structure
;;       | (and pat_1 ... pat_n)           if all of pat_1 thru pat_n match
;;       | (or pat_1 ... pat_n)            if any of pat_1 thru pat_n match
;;       | (not pat_1 ... pat_n)           if all pat_1 thru pat_n don't match
;;       | (? predicate pat_1 ... pat_n)   if predicate true and all of
;;                                           pat_1 thru pat_n match
;;       | (set! identifier)               anything, and binds setter
;;       | (get! identifier)               anything, and binds getter
;;       | `qp                             a quasi-pattern
;;
;; ooo ::= ...                             zero or more
;;       | ___                             zero or more
;;       | ..k                             k or more
;;       | __k                             k or more
;;
;;         quasi-patterns:                 matches:
;;
;; qp  ::= ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | identifier                      a symbol
;;       | (qp_1 ... qp_n)                 list of n elements
;;       | (qp_1 ... qp_n . qp_{n+1})      list of n or more
;;       | (qp_1 ... qp_n qp_n+1 ooo)      list of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #(qp_1 ... qp_n)                vector of n elements
;;       | #(qp_1 ... qp_n qp_n+1 ooo)     vector of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #&qp                            box
;;       | ,pat                            a pattern
;;       | ,@pat                           a pattern
;;
;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
;; and, or, not, set!, get!, ..., ___) cannot be used as pattern variables.
;;
;;
;;         structure expressions:
;;
;; exp ::= ...
;;       | (define-structure (id_0 id_1 ... id_n))
;;       | (define-structure (id_0 id_1 ... id_n)
;;                           ((id_{n+1} exp_1) ... (id_{n+m} exp_m)))
;;       | (define-const-structure (id_0 arg_1 ... arg_n))
;;       | (define-const-structure (id_0 arg_1 ... arg_n)
;;                                 ((arg_{n+1} exp_1) ... (arg_{n+m} exp_m)))
;;
;; arg ::= id | (! id) | (@ id)
;;
;;
;; match:error-control controls what code is generated for failed matches.
;; Possible values:
;;  'unspecified - do nothing, ie., evaluate (cond [#f #f])
;;  'fail - call match:error, or die at car or cdr
;;  'error - call match:error with the unmatched value
;;  'match - call match:error with the unmatched value _and_
;;             the quoted match expression
;; match:error-control is set by calling match:set-error-control with
;; the new value.
;;
;; match:error is called for a failed match.
;; match:error is set by calling match:set-error with the new value.
;;
;; match:structure-control controls the uniqueness of structures
;; (does not exist for Scheme 48 version).
;; Possible values:
;;  'vector - (default) structures are vectors with a symbol in position 0
;;  'disjoint - structures are fully disjoint from all other values
;; match:structure-control is set by calling match:set-structure-control
;; with the new value.
;;
;; match:runtime-structures controls whether local structure declarations
;; generate new structures each time they are reached
;; (does not exist for Scheme 48 version).
;; Possible values:
;;  #t - (default) each runtime occurrence generates a new structure
;;  #f - each lexical occurrence generates a new structure
;;
;; End of user visible/modifiable stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (match (1 18))
  (export
    match
    match-lambda
    match-lambda*
    match-let
    match-let*
    match-letrec
    match-define
    #|defstruct
    define-structure
    define-const-structure|#
    match:error
    ;match:set-error
    match:set-error-control)
  (import 
    (rnrs base)
    (only (ikarus) gensym)
    ;(for (match macro-helpers) expand)  ;;; Ikarus doesn't (yet?) support this <import-spec>
    (match macro-helpers))               ;;; Ikarus is implicitly phased
  
  
  (defmacro
    match
    args
    (cond
      ((and (list? args)
            (<= 1 (length args))
            (match:andmap
             (lambda (y) (and (list? y) (<= 2 (length y))))
             (cdr args))) (let* ((exp (car args))
                                 (clauses (cdr args))
                                 (e (if (symbol? exp) exp (gensym))))
                            (if (symbol? exp)
                              ((car match:expanders)
                               e
                               clauses
                               `(match ,@args))
                              `(let ((,e ,exp))
                                 ,((car match:expanders)
                                   e
                                   clauses
                                   `(match ,@args))))))
      (else (match:syntax-err `(match ,@args) "syntax error in"))))
  
  (defmacro
    match-lambda
    args
    (if (and (list? args)
             (match:andmap
              (lambda (g126)
                (if (and (pair? g126) (list? (cdr g126)))
                  (pair? (cdr g126))
                  #f))
              args))
      ((lambda ()
         (let ((e (gensym))) `(lambda (,e) (match ,e ,@args)))))
      ((lambda ()
         (match:syntax-err
          `(match-lambda ,@args)
          "syntax error in")))))
  
  (defmacro
    match-lambda*
    args
    (if (and (list? args)
             (match:andmap
              (lambda (g134)
                (if (and (pair? g134) (list? (cdr g134)))
                  (pair? (cdr g134))
                  #f))
              args))
      ((lambda ()
         (let ((e (gensym))) `(lambda ,e (match ,e ,@args)))))
      ((lambda ()
         (match:syntax-err
          `(match-lambda* ,@args)
          "syntax error in")))))
  
  (defmacro
    match-let
    args
    (let ((g158 (lambda (pat exp body)
                  `(match ,exp (,pat ,@body))))
          (g154 (lambda (pat exp body)
                  (let ((g (map (lambda (x) (gensym)) pat))
                        (vpattern (list->vector pat)))
                    `(let ,(map list g exp)
                       (match (vector ,@g) (,vpattern ,@body))))))
          (g146 (lambda ()
                  (match:syntax-err `(match-let ,@args) "syntax error in")))
          (g145 (lambda (p1 e1 p2 e2 body)
                  (let ((g1 (gensym)) (g2 (gensym)))
                    `(let ((,g1 ,e1) (,g2 ,e2))
                       (match (cons ,g1 ,g2) ((,p1 . ,p2) ,@body))))))
          (g136 (cadddr match:expanders)))
      (if (pair? args)
        (if (symbol? (car args))
          (if (and (pair? (cdr args)) (list? (cadr args)))
            (let g161 ((g162 (cadr args)) (g160 '()) (g159 '()))
              (if (null? g162)
                (if (and (list? (cddr args)) (pair? (cddr args)))
                  ((lambda (name pat exp body)
                     (if (match:andmap
                          (cadddr match:expanders)
                          pat)
                       `(let ,@args)
                       `(letrec ((,name (match-lambda*
                                         (,pat ,@body))))
                          (,name ,@exp))))
                   (car args)
                   (reverse g159)
                   (reverse g160)
                   (cddr args))
                  (g146))
                (if (and (pair? (car g162))
                         (pair? (cdar g162))
                         (null? (cddar g162)))
                  (g161 (cdr g162)
                        (cons (cadar g162) g160)
                        (cons (caar g162) g159))
                  (g146))))
            (g146))
          (if (list? (car args))
            (if (match:andmap
                 (lambda (g167)
                   (if (and (pair? g167)
                            (g136 (car g167))
                            (pair? (cdr g167)))
                     (null? (cddr g167))
                     #f))
                 (car args))
              (if (and (list? (cdr args)) (pair? (cdr args)))
                ((lambda () `(let ,@args)))
                (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                  (if (null? g150)
                    (g146)
                    (if (and (pair? (car g150))
                             (pair? (cdar g150))
                             (null? (cddar g150)))
                      (g149 (cdr g150)
                            (cons (cadar g150) g148)
                            (cons (caar g150) g147))
                      (g146)))))
              (if (and (pair? (car args))
                       (pair? (caar args))
                       (pair? (cdaar args))
                       (null? (cddaar args)))
                (if (null? (cdar args))
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g158 (caaar args)
                          (cadaar args)
                          (cdr args))
                    (let g149 ((g150 (car args))
                               (g148 '())
                               (g147 '()))
                      (if (null? g150)
                        (g146)
                        (if (and (pair? (car g150))
                                 (pair? (cdar g150))
                                 (null? (cddar g150)))
                          (g149 (cdr g150)
                                (cons (cadar g150) g148)
                                (cons (caar g150) g147))
                          (g146)))))
                  (if (and (pair? (cdar args))
                           (pair? (cadar args))
                           (pair? (cdadar args))
                           (null? (cdr (cdadar args)))
                           (null? (cddar args)))
                    (if (and (list? (cdr args))
                             (pair? (cdr args)))
                      (g145 (caaar args)
                            (cadaar args)
                            (caadar args)
                            (car (cdadar args))
                            (cdr args))
                      (let g149 ((g150 (car args))
                                 (g148 '())
                                 (g147 '()))
                        (if (null? g150)
                          (g146)
                          (if (and (pair? (car g150))
                                   (pair? (cdar g150))
                                   (null? (cddar g150)))
                            (g149 (cdr g150)
                                  (cons (cadar g150)
                                        g148)
                                  (cons (caar g150)
                                        g147))
                            (g146)))))
                    (let g149 ((g150 (car args))
                               (g148 '())
                               (g147 '()))
                      (if (null? g150)
                        (if (and (list? (cdr args))
                                 (pair? (cdr args)))
                          (g154 (reverse g147)
                                (reverse g148)
                                (cdr args))
                          (g146))
                        (if (and (pair? (car g150))
                                 (pair? (cdar g150))
                                 (null? (cddar g150)))
                          (g149 (cdr g150)
                                (cons (cadar g150) g148)
                                (cons (caar g150) g147))
                          (g146))))))
                (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                  (if (null? g150)
                    (if (and (list? (cdr args))
                             (pair? (cdr args)))
                      (g154 (reverse g147)
                            (reverse g148)
                            (cdr args))
                      (g146))
                    (if (and (pair? (car g150))
                             (pair? (cdar g150))
                             (null? (cddar g150)))
                      (g149 (cdr g150)
                            (cons (cadar g150) g148)
                            (cons (caar g150) g147))
                      (g146))))))
            (if (pair? (car args))
              (if (and (pair? (caar args))
                       (pair? (cdaar args))
                       (null? (cddaar args)))
                (if (null? (cdar args))
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g158 (caaar args)
                          (cadaar args)
                          (cdr args))
                    (let g149 ((g150 (car args))
                               (g148 '())
                               (g147 '()))
                      (if (null? g150)
                        (g146)
                        (if (and (pair? (car g150))
                                 (pair? (cdar g150))
                                 (null? (cddar g150)))
                          (g149 (cdr g150)
                                (cons (cadar g150) g148)
                                (cons (caar g150) g147))
                          (g146)))))
                  (if (and (pair? (cdar args))
                           (pair? (cadar args))
                           (pair? (cdadar args))
                           (null? (cdr (cdadar args)))
                           (null? (cddar args)))
                    (if (and (list? (cdr args))
                             (pair? (cdr args)))
                      (g145 (caaar args)
                            (cadaar args)
                            (caadar args)
                            (car (cdadar args))
                            (cdr args))
                      (let g149 ((g150 (car args))
                                 (g148 '())
                                 (g147 '()))
                        (if (null? g150)
                          (g146)
                          (if (and (pair? (car g150))
                                   (pair? (cdar g150))
                                   (null? (cddar g150)))
                            (g149 (cdr g150)
                                  (cons (cadar g150)
                                        g148)
                                  (cons (caar g150)
                                        g147))
                            (g146)))))
                    (let g149 ((g150 (car args))
                               (g148 '())
                               (g147 '()))
                      (if (null? g150)
                        (if (and (list? (cdr args))
                                 (pair? (cdr args)))
                          (g154 (reverse g147)
                                (reverse g148)
                                (cdr args))
                          (g146))
                        (if (and (pair? (car g150))
                                 (pair? (cdar g150))
                                 (null? (cddar g150)))
                          (g149 (cdr g150)
                                (cons (cadar g150) g148)
                                (cons (caar g150) g147))
                          (g146))))))
                (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                  (if (null? g150)
                    (if (and (list? (cdr args))
                             (pair? (cdr args)))
                      (g154 (reverse g147)
                            (reverse g148)
                            (cdr args))
                      (g146))
                    (if (and (pair? (car g150))
                             (pair? (cdar g150))
                             (null? (cddar g150)))
                      (g149 (cdr g150)
                            (cons (cadar g150) g148)
                            (cons (caar g150) g147))
                      (g146)))))
              (g146))))
        (g146))))
  
  (defmacro
    match-let*
    args
    (let ((g176 (lambda ()
                  (match:syntax-err `(match-let* ,@args) "syntax error in"))))
      (if (pair? args)
        (if (null? (car args))
          (if (and (list? (cdr args)) (pair? (cdr args)))
            ((lambda (body) `(let* ,@args)) (cdr args))
            (g176))
          (if (and (pair? (car args))
                   (pair? (caar args))
                   (pair? (cdaar args))
                   (null? (cddaar args))
                   (list? (cdar args))
                   (list? (cdr args))
                   (pair? (cdr args)))
            ((lambda (pat exp rest body)
               (if ((cadddr match:expanders) pat)
                 `(let ((,pat ,exp)) (match-let* ,rest ,@body))
                 `(match ,exp (,pat (match-let* ,rest ,@body)))))
             (caaar args)
             (cadaar args)
             (cdar args)
             (cdr args))
            (g176)))
        (g176))))
  
  (defmacro
    match-letrec
    args
    (let ((g200 (cadddr match:expanders))
          (g199 (lambda (p1 e1 p2 e2 body)
                  `(match-letrec (((,p1 . ,p2) (cons ,e1 ,e2))) ,@body)))
          (g195 (lambda ()
                  (match:syntax-err
                   `(match-letrec ,@args)
                   "syntax error in")))
          (g194 (lambda (pat exp body)
                  `(match-letrec
                    ((,(list->vector pat) (vector ,@exp)))
                    ,@body)))
          (g186 (lambda (pat exp body)
                  ((cadr match:expanders)
                   pat
                   exp
                   body
                   `(match-letrec ((,pat ,exp)) ,@body)))))
      (if (pair? args)
        (if (list? (car args))
          (if (match:andmap
               (lambda (g206)
                 (if (and (pair? g206)
                          (g200 (car g206))
                          (pair? (cdr g206)))
                   (null? (cddr g206))
                   #f))
               (car args))
            (if (and (list? (cdr args)) (pair? (cdr args)))
              ((lambda () `(letrec ,@args)))
              (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                (if (null? g190)
                  (g195)
                  (if (and (pair? (car g190))
                           (pair? (cdar g190))
                           (null? (cddar g190)))
                    (g189 (cdr g190)
                          (cons (cadar g190) g188)
                          (cons (caar g190) g187))
                    (g195)))))
            (if (and (pair? (car args))
                     (pair? (caar args))
                     (pair? (cdaar args))
                     (null? (cddaar args)))
              (if (null? (cdar args))
                (if (and (list? (cdr args)) (pair? (cdr args)))
                  (g186 (caaar args) (cadaar args) (cdr args))
                  (let g189 ((g190 (car args))
                             (g188 '())
                             (g187 '()))
                    (if (null? g190)
                      (g195)
                      (if (and (pair? (car g190))
                               (pair? (cdar g190))
                               (null? (cddar g190)))
                        (g189 (cdr g190)
                              (cons (cadar g190) g188)
                              (cons (caar g190) g187))
                        (g195)))))
                (if (and (pair? (cdar args))
                         (pair? (cadar args))
                         (pair? (cdadar args))
                         (null? (cdr (cdadar args)))
                         (null? (cddar args)))
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g199 (caaar args)
                          (cadaar args)
                          (caadar args)
                          (car (cdadar args))
                          (cdr args))
                    (let g189 ((g190 (car args))
                               (g188 '())
                               (g187 '()))
                      (if (null? g190)
                        (g195)
                        (if (and (pair? (car g190))
                                 (pair? (cdar g190))
                                 (null? (cddar g190)))
                          (g189 (cdr g190)
                                (cons (cadar g190) g188)
                                (cons (caar g190) g187))
                          (g195)))))
                  (let g189 ((g190 (car args))
                             (g188 '())
                             (g187 '()))
                    (if (null? g190)
                      (if (and (list? (cdr args))
                               (pair? (cdr args)))
                        (g194 (reverse g187)
                              (reverse g188)
                              (cdr args))
                        (g195))
                      (if (and (pair? (car g190))
                               (pair? (cdar g190))
                               (null? (cddar g190)))
                        (g189 (cdr g190)
                              (cons (cadar g190) g188)
                              (cons (caar g190) g187))
                        (g195))))))
              (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                (if (null? g190)
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g194 (reverse g187)
                          (reverse g188)
                          (cdr args))
                    (g195))
                  (if (and (pair? (car g190))
                           (pair? (cdar g190))
                           (null? (cddar g190)))
                    (g189 (cdr g190)
                          (cons (cadar g190) g188)
                          (cons (caar g190) g187))
                    (g195))))))
          (if (pair? (car args))
            (if (and (pair? (caar args))
                     (pair? (cdaar args))
                     (null? (cddaar args)))
              (if (null? (cdar args))
                (if (and (list? (cdr args)) (pair? (cdr args)))
                  (g186 (caaar args) (cadaar args) (cdr args))
                  (let g189 ((g190 (car args))
                             (g188 '())
                             (g187 '()))
                    (if (null? g190)
                      (g195)
                      (if (and (pair? (car g190))
                               (pair? (cdar g190))
                               (null? (cddar g190)))
                        (g189 (cdr g190)
                              (cons (cadar g190) g188)
                              (cons (caar g190) g187))
                        (g195)))))
                (if (and (pair? (cdar args))
                         (pair? (cadar args))
                         (pair? (cdadar args))
                         (null? (cdr (cdadar args)))
                         (null? (cddar args)))
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g199 (caaar args)
                          (cadaar args)
                          (caadar args)
                          (car (cdadar args))
                          (cdr args))
                    (let g189 ((g190 (car args))
                               (g188 '())
                               (g187 '()))
                      (if (null? g190)
                        (g195)
                        (if (and (pair? (car g190))
                                 (pair? (cdar g190))
                                 (null? (cddar g190)))
                          (g189 (cdr g190)
                                (cons (cadar g190) g188)
                                (cons (caar g190) g187))
                          (g195)))))
                  (let g189 ((g190 (car args))
                             (g188 '())
                             (g187 '()))
                    (if (null? g190)
                      (if (and (list? (cdr args))
                               (pair? (cdr args)))
                        (g194 (reverse g187)
                              (reverse g188)
                              (cdr args))
                        (g195))
                      (if (and (pair? (car g190))
                               (pair? (cdar g190))
                               (null? (cddar g190)))
                        (g189 (cdr g190)
                              (cons (cadar g190) g188)
                              (cons (caar g190) g187))
                        (g195))))))
              (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                (if (null? g190)
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g194 (reverse g187)
                          (reverse g188)
                          (cdr args))
                    (g195))
                  (if (and (pair? (car g190))
                           (pair? (cdar g190))
                           (null? (cddar g190)))
                    (g189 (cdr g190)
                          (cons (cadar g190) g188)
                          (cons (caar g190) g187))
                    (g195)))))
            (g195)))
        (g195))))
  
  (defmacro
    match-define
    args
    (let ((g210 (cadddr match:expanders))
          (g209 (lambda ()
                  (match:syntax-err
                   `(match-define ,@args)
                   "syntax error in"))))
      (if (pair? args)
        (if (g210 (car args))
          (if (and (pair? (cdr args)) (null? (cddr args)))
            ((lambda () `(begin (define ,@args))))
            (g209))
          (if (and (pair? (cdr args)) (null? (cddr args)))
            ((lambda (pat exp)
               ((caddr match:expanders)
                pat
                exp
                `(match-define ,@args)))
             (car args)
             (cadr args))
            (g209)))
        (g209))))
  
#|(define match:runtime-structures #f)
  (define match:set-runtime-structures
    (lambda (v) (set! match:runtime-structures v)))
  (define match:primitive-vector? vector?)
  (defmacro
    defstruct
    args
    (let ((field? (lambda (x)
                    (if (symbol? x)
                      ((lambda () #t))
                      (if (and (pair? x)
                               (symbol? (car x))
                               (pair? (cdr x))
                               (symbol? (cadr x))
                               (null? (cddr x)))
                        ((lambda () #t))
                        ((lambda () #f))))))
          (selector-name (lambda (x)
                           (if (symbol? x)
                             ((lambda () x))
                             (if (and (pair? x)
                                      (symbol? (car x))
                                      (pair? (cdr x))
                                      (null? (cddr x)))
                               ((lambda (s) s) (car x))
                               (match:error x)))))
          (mutator-name (lambda (x)
                          (if (symbol? x)
                            ((lambda () #f))
                            (if (and (pair? x)
                                     (pair? (cdr x))
                                     (symbol? (cadr x))
                                     (null? (cddr x)))
                              ((lambda (s) s) (cadr x))
                              (match:error x)))))
          (filter-map-with-index (lambda (f l)
                                   (letrec ((mapi (lambda (l i)
                                                    (cond
                                                      ((null? l) '())
                                                      ((f (car l) i) =>
                                                                     (lambda (x)
                                                                       (cons x
                                                                             (mapi (cdr l)
                                                                                   (+ 1
                                                                                      i)))))
                                                      (else (mapi (cdr l)
                                                                  (+ 1 i)))))))
                                     (mapi l 1)))))
      (let ((g227 (lambda ()
                    (match:syntax-err `(defstruct ,@args) "syntax error in"))))
        (if (and (pair? args)
                 (symbol? (car args))
                 (pair? (cdr args))
                 (symbol? (cadr args))
                 (pair? (cddr args))
                 (symbol? (caddr args))
                 (list? (cdddr args)))
          (let g229 ((g230 (cdddr args)) (g228 '()))
            (if (null? g230)
              ((lambda (name constructor predicate fields)
                 (let* ((selectors (map selector-name fields))
                        (mutators (map mutator-name fields))
                        (tag (if match:runtime-structures
                               (gensym)
                               `',(match:make-structure-tag name)))
                        (vectorP (cond
                                   ((eq? match:structure-control
                                         'disjoint) 'match:primitive-vector?)
                                   ((eq? match:structure-control 'vector) 'vector?))))
                   (cond
                     ((eq? match:structure-control 'disjoint) (if (eq? vector?
                                                                       match:primitive-vector?)
                                                                (set! vector?
                                                                      (lambda (v)
                                                                        (and (match:primitive-vector?
                                                                              v)
                                                                             (or (zero?
                                                                                  (vector-length
                                                                                   v))
                                                                                 (not (symbol?
                                                                                       (vector-ref
                                                                                        v
                                                                                        0)))
                                                                                 (not (match:structure?
                                                                                       (vector-ref
                                                                                        v
                                                                                        0))))))))
                                                              (if (not (memq predicate
                                                                             match:disjoint-predicates))
                                                                (set! match:disjoint-predicates
                                                                      (cons predicate match:disjoint-predicates))))
                     ((eq? match:structure-control 'vector) (if (not (memq predicate
                                                                           match:vector-structures))
                                                              (set! match:vector-structures
                                                                    (cons predicate
                                                                          match:vector-structures))))
                     (else (match:syntax-err
                            '(vector disjoint)
                            "invalid value for match:structure-control, legal values are")))
                   `(begin ,@(if match:runtime-structures
                               `((define ,tag
                                   (match:make-structure-tag ',name)))
                               '())
                           (define ,constructor
                             (lambda ,selectors
                               (vector ,tag ,@selectors)))
                           (define ,predicate
                             (lambda (obj)
                               (and (,vectorP obj)
                                    (= (vector-length obj)
                                       ,(+ 1 (length selectors)))
                                    (eq? (vector-ref obj 0) ,tag))))
                           ,@(filter-map-with-index
                              (lambda (n i)
                                `(define ,n
                                   (lambda (obj) (vector-ref obj ,i))))
                              selectors)
                           ,@(filter-map-with-index
                              (lambda (n i)
                                (and n
                                     `(define ,n
                                        (lambda (obj newval)
                                          (vector-set!
                                           obj
                                           ,i
                                           newval)))))
                              mutators))))
               (car args)
               (cadr args)
               (caddr args)
               (reverse g228))
              (if (field? (car g230))
                (g229 (cdr g230) (cons (car g230) g228))
                (g227))))
          (g227)))))
  (defmacro
    define-structure
    args
    (let ((g242 (lambda ()
                  (match:syntax-err
                   `(define-structure ,@args)
                   "syntax error in"))))
      (if (and (pair? args)
               (pair? (car args))
               (list? (cdar args)))
        (if (null? (cdr args))
          ((lambda (name id1) `(define-structure (,name ,@id1) ()))
           (caar args)
           (cdar args))
          (if (and (pair? (cdr args)) (list? (cadr args)))
            (let g239 ((g240 (cadr args)) (g238 '()) (g237 '()))
              (if (null? g240)
                (if (null? (cddr args))
                  ((lambda (name id1 id2 val)
                     (let ((mk-id (lambda (id)
                                    (if (and (pair? id)
                                             (equal? (car id) '@)
                                             (pair? (cdr id))
                                             (symbol? (cadr id))
                                             (null? (cddr id)))
                                      ((lambda (x) x) (cadr id))
                                      ((lambda () `(! ,id)))))))
                       `(define-const-structure
                          (,name ,@(map mk-id id1))
                          ,(map (lambda (id v) `(,(mk-id id) ,v))
                                id2
                                val))))
                   (caar args)
                   (cdar args)
                   (reverse g237)
                   (reverse g238))
                  (g242))
                (if (and (pair? (car g240))
                         (pair? (cdar g240))
                         (null? (cddar g240)))
                  (g239 (cdr g240)
                        (cons (cadar g240) g238)
                        (cons (caar g240) g237))
                  (g242))))
            (g242)))
        (g242))))
  (defmacro
    define-const-structure
    args
    (let ((field? (lambda (id)
                    (if (symbol? id)
                      ((lambda () #t))
                      (if (and (pair? id)
                               (equal? (car id) '!)
                               (pair? (cdr id))
                               (symbol? (cadr id))
                               (null? (cddr id)))
                        ((lambda () #t))
                        ((lambda () #f))))))
          (field-name (lambda (x) (if (symbol? x) x (cadr x))))
          (has-mutator? (lambda (x) (not (symbol? x))))
          (filter-map-with-index (lambda (f l)
                                   (letrec ((mapi (lambda (l i)
                                                    (cond
                                                      ((null? l) '())
                                                      ((f (car l) i) =>
                                                                     (lambda (x)
                                                                       (cons x
                                                                             (mapi (cdr l)
                                                                                   (+ 1
                                                                                      i)))))
                                                      (else (mapi (cdr l)
                                                                  (+ 1 i)))))))
                                     (mapi l 1))))
          (symbol-append (lambda l
                           (string->symbol
                            (apply
                             string-append
                             (map (lambda (x)
                                    (cond
                                      ((symbol? x) (symbol->string x))
                                      ((number? x) (number->string x))
                                      (else x)))
                                  l))))))
      (let ((g266 (lambda ()
                    (match:syntax-err
                     `(define-const-structure ,@args)
                     "syntax error in"))))
        (if (and (pair? args)
                 (pair? (car args))
                 (list? (cdar args)))
          (if (null? (cdr args))
            ((lambda (name id1)
               `(define-const-structure (,name ,@id1) ()))
             (caar args)
             (cdar args))
            (if (symbol? (caar args))
              (let g259 ((g260 (cdar args)) (g258 '()))
                (if (null? g260)
                  (if (and (pair? (cdr args)) (list? (cadr args)))
                    (let g263 ((g264 (cadr args))
                               (g262 '())
                               (g261 '()))
                      (if (null? g264)
                        (if (null? (cddr args))
                          ((lambda (name id1 id2 val)
                             (let* ((id1id2 (append id1 id2))
                                    (raw-constructor (symbol-append
                                                      'make-raw-
                                                      name))
                                    (constructor (symbol-append
                                                  'make-
                                                  name))
                                    (predicate (symbol-append
                                                name
                                                '?)))
                               `(begin (defstruct
                                         ,name
                                         ,raw-constructor
                                         ,predicate
                                         ,@(filter-map-with-index
                                            (lambda (arg i)
                                              (if (has-mutator?
                                                   arg)
                                                `(,(symbol-append
                                                    name
                                                    '-
                                                    i)
                                                  ,(symbol-append
                                                    'set-
                                                    name
                                                    '-
                                                    i
                                                    '!))
                                                (symbol-append
                                                 name
                                                 '-
                                                 i)))
                                            id1id2))
                                       ,(let* ((make-fresh (lambda (x)
                                                             (if (eq? '_
                                                                      x)
                                                               (gensym)
                                                               x)))
                                               (names1 (map make-fresh
                                                            (map field-name
                                                                 id1)))
                                               (names2 (map make-fresh
                                                            (map field-name
                                                                 id2))))
                                          `(define ,constructor
                                             (lambda ,names1
                                               (let* ,(map list
                                                           names2
                                                           val)
                                                 (,raw-constructor
                                                  ,@names1
                                                  ,@names2)))))
                                       ,@(filter-map-with-index
                                          (lambda (field i)
                                            (if (eq? (field-name
                                                      field)
                                                     '_)
                                              #f
                                              `(define ,(symbol-append
                                                         name
                                                         '-
                                                         (field-name
                                                          field))
                                                 ,(symbol-append
                                                   name
                                                   '-
                                                   i))))
                                          id1id2)
                                       ,@(filter-map-with-index
                                          (lambda (field i)
                                            (if (or (eq? (field-name
                                                          field)
                                                         '_)
                                                    (not (has-mutator?
                                                          field)))
                                              #f
                                              `(define ,(symbol-append
                                                         'set-
                                                         name
                                                         '-
                                                         (field-name
                                                          field)
                                                         '!)
                                                 ,(symbol-append
                                                   'set-
                                                   name
                                                   '-
                                                   i
                                                   '!))))
                                          id1id2))))
                           (caar args)
                           (reverse g258)
                           (reverse g261)
                           (reverse g262))
                          (g266))
                        (if (and (pair? (car g264))
                                 (field? (caar g264))
                                 (pair? (cdar g264))
                                 (null? (cddar g264)))
                          (g263 (cdr g264)
                                (cons (cadar g264) g262)
                                (cons (caar g264) g261))
                          (g266))))
                    (g266))
                  (if (field? (car g260))
                    (g259 (cdr g260) (cons (car g260) g258))
                    (g266))))
              (g266)))
          (g266))))) |#
  
)
