#!r6rs
(library (xitomatl lists)
  (export
    map/left-right/preserving
    rem-dups
    intersperse
    list-of)
  (import
    (rnrs)
    (only (xitomatl define extras) define/?))
  
  ; deterministic, left-to-right map
  ; It preserves sharing as much as possible: that is, if given the pair
  ; l == (cons h t), (and (eq? h (f h)) (eq? t (map/left-right/preserving f t))) holds, then
  ; (eq? (map/left-right/preserving f l) l) holds as well.
  (define/? (map/left-right/preserving [f procedure?] [l list?])
    (let loop ([f f] [l l])
      (if (null? l)
        l
        (let ([h (car l)] [t (cdr l)])
          (let ([h1 (f h)] [t1 (loop f t)])
            (if (and (eq? h1 h) (eq? t1 t)) 
              l
              (cons h1 t1)))))))

  (define/? (rem-dups [l list?])
    (let loop ([l l] [n '()])
      (if (null? l)
        (reverse n)
        (loop (remove (car l) (cdr l))
              (cons (car l) n)) )))
  
  (define/? (intersperse [l list?] sep)
    (let loop ([l l] [sep sep])
      (cond
        [(null? l) '()]
        [(null? (cdr l)) l]
        [else (cons* (car l) sep (loop (cdr l) sep))])))
  
  (define-syntax list-of
    (syntax-rules ()      
      [(_ expr clauses ...)
       (list-of-aux expr '() clauses ...)]))
  
  (define-syntax list-of-aux
    ;;; Modified from Phil Bewig's list-of, from:
    ;;; http://groups.google.com/group/comp.lang.scheme/msg/18df0b4cc3939ef0
    (syntax-rules (:range :in :is)
      [(_ expr base)
       (cons expr base)]
      [(_ expr base (x :range first past step) clauses ...)
       (let ([f first] [p past])
         (let* ([s (let-syntax ([SM (syntax-rules ()
                                      [(_ #f) (if (< f p) 1 -1)]
                                      [(_ s) s])]) 
                     (SM step))]
                [more? (cond [(positive? s) <] 
                             [(negative? s) >]
                             [else (assertion-violation 'list-of 
                                     "step must not be zero" s)])])
           (let loop ([z f])
             (if (more? z p)
               (let ([x z])
                 (list-of-aux expr (loop (+ z s)) clauses ...))
               base))))]
      [(_ expr base (x :range first past) clauses ...)
       (list-of-aux expr base (x :range first past #f) clauses ...)]
      [(_ expr base (x :range past) clauses ...)
       (list-of-aux expr base (x :range 0 past) clauses ...)]
      [(_ expr base (x :in xs) clauses ...)
       (let loop ([z xs])
         (if (null? z)
           base
           (let ([x (car z)])
             (list-of-aux expr (loop (cdr z)) clauses ...))))]
      [(_ expr base (x :is y) clauses ...)
       (let ([x y])
         (list-of-aux expr base clauses ...))]
      [(_ expr base pred clauses ...)
       (if pred
         (list-of-aux expr base clauses ...)
         base)]))
  
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
