#!r6rs
(library (xitomatl lists)
  (export
    map/left-right/preserving
    rem-dups
    intersperse
    list-of
    list-match)
  (import
    (rnrs))
  
  ; deterministic, left-to-right map
  ; It preserves sharing as much as possible: that is, if given the pair
  ; l == (cons h t), (and (eq? h (f h)) (eq? t (map/left-right/preserving f t))) holds, then
  ; (eq? (map/left-right/preserving f l) l) holds as well.
  (define (map/left-right/preserving f l)
    (if (null? l) 
        l
        (let ([h (car l)] [t (cdr l)])
          (let ([h1 (f h)] [t1 (map/left-right/preserving f t)])
            (if (and (eq? h1 h) (eq? t1 t)) 
                l
                (cons h1 t1))))))

  (define (rem-dups l)
    (let loop ([l l] [n '()])
      (if (null? l)
        (reverse n)
        (loop (remove (car l) (cdr l))
              (cons (car l) n)) )))
  
  (define (intersperse l sep)
    #;(if (null? l)   ;; tail recursive version, seems a lot slower
      '()
      (let loop ([l l] [accum '()])
        (if (null? (cdr l)) 
          (reverse (cons (car l) accum))         
          (loop (cdr l) (cons* sep (car l) accum)))))
    (cond
      [(null? l) '()]
      [(null? (cdr l)) l]
      [else (cons* (car l) sep (intersperse (cdr l) sep))]))
  
  (define-syntax list-of
    (syntax-rules ()      
      [(_ expr clauses ...)
       (list-of-aux expr '() clauses ...)]))
  
  (define-syntax list-of-aux
    ;;; Modified from Phil Bewig's list-of, from:
    ;;; http://groups.google.com/group/comp.lang.scheme/msg/18df0b4cc3939ef0
    (lambda (stx)
      (syntax-case stx (:range :in :is)
        [(_ expr base)
         #'(cons expr base)]
        [(_ expr base (x :range first past step) clauses ...)
         (identifier? #'x)
         #'(let ([f first] [p past])
             (let* ([s (let-syntax ([SM (syntax-rules ()
                                          [(_ #f) (if (< f p) 1 -1)]
                                          [(_ s) s])]) 
                         (SM step))]
                    [more? (if (positive? s) < >)])
               (let loop ([z f])
                 (if (more? z p)
                   (let ([x z])
                     (list-of-aux expr (loop (+ z s)) clauses ...))
                   base))))]
        [(_ expr base (x :range first past) clauses ...)
         #'(list-of-aux expr base (x :range first past #f) clauses ...)]
        [(_ expr base (x :range past) clauses ...)
         #'(list-of-aux expr base (x :range 0 past) clauses ...)]
        [(_ expr base (x :in xs) clauses ...)
         (identifier? #'x)
         #'(let loop ([z xs])
             (if (null? z)
               base
               (let ([x (car z)])
                 (list-of-aux expr (loop (cdr z)) clauses ...))))]
        [(_ expr base (x :is y) clauses ...)
         (identifier? #'x)
         #'(let ([x y])
             (list-of-aux expr base clauses ...))]
        [(_ expr base (pred pe pe* ...) clauses ...)
         #'(if (pred pe pe* ...)
             (list-of-aux expr base clauses ...)
             base)]
        [(_ expr base clauses ...)
         (syntax-violation #f "invalid syntax" #'(list-of expr clauses ...))])))
  
  (define (list-match-failed obj)
    (assertion-violation 'list-match "failed to match" obj))
  
  (define (identity x) x)
  
  (define-syntax list-match
    ;;; Phil Bewig's list-match, from:
    ;;; http://groups.google.com/group/comp.lang.scheme/msg/18df0b4cc3939ef0
    (syntax-rules ()
      [(_ expr (pattern fender ... template) ...)
       (let ([obj expr])
         (cond [(list-match-aux obj pattern fender ... template) 
                => identity] 
               ...
               [else (list-match-failed obj)]))]))
  
  (define-syntax list-match-aux
    (lambda (stx)
      (define (underscore? x)
        (and (identifier? x) (bound-identifier=? x #'_)))
      (syntax-case stx (quote quasiquote)
        [(_ obj pattern template)
         #'(list-match-aux obj pattern #t template)]
        [(_ obj () fender template)
         #'(and (null? obj) fender template)]
        [(_ obj underscore fender template)
         (underscore? #'underscore)
         #'(and fender template)]
        [(_ obj var fender template)
         (identifier? #'var)
         #'(let ([var obj]) 
             (and fender template))]
        [(_ obj (quote datum) fender template)
         #'(and (equal? obj (quote datum)) template)]
        [(_ obj (quasiquote datum) fender template)
         #'(and (equal? obj (quasiquote datum)) fender template)]
        [(_ obj (kar . kdr) fender template)
         #'(and (pair? obj)
                (let ([kar-obj (car obj)] [kdr-obj (cdr obj)])
                  (list-match-aux kar-obj kar
                    (list-match-aux kdr-obj kdr fender template))))]
        [(_ obj const fender template)
         #'(and (equal? obj const) fender template)])))

)
