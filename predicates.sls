#!r6rs
(library (xitomatl predicates)
  (export
    non-negative-integer?
    exact-non-negative-integer?
    positive-integer?
    exact-positive-integer?
    exact-integer?
    symbol<?
    name=?
    non-empty-string?
    char-line-ending?
    list-of?
    #;datum?)
  (import
    (rnrs))
  
  (define (non-negative-integer? x)
    (and (integer? x) (not (negative? x))))

  (define (exact-non-negative-integer? x)
    (and (integer? x) (exact? x) (not (negative? x))))
  
  (define (positive-integer? x)
    (and (integer? x) (positive? x)))
  
  (define (exact-positive-integer? x)
    (and (integer? x) (exact? x) (positive? x)))
  
  (define (exact-integer? x)
    (and (integer? x) (exact? x)))

  ;;--------------------------------------------------------------------------
  
  (define (symbol<? x y . r)
    (apply string<? (map symbol->string (cons* x y r))))
  
  (define (name=? x y . r)
    (apply symbol=? 
           (map (lambda (n) 
                  (cond [(identifier? n) (syntax->datum n)]
                        [(symbol? n) n]
                        [(string? n) (string->symbol n)]
                        [else (assertion-violation 'name=? 
                               "not an identifier, symbol, or string" n)]))
                (cons* x y r))))
  
  (define (non-empty-string? x)
    (and (string? x) (positive? (string-length x))))
  
  (define (char-line-ending? c)
    (and (memv c '(#\xa #\xd #\x85 #\x2028))
         #t))

  ;;--------------------------------------------------------------------------
  
  (define (list-of? pred)
    (letrec ([list-of?-pred
              (lambda (x)
                (cond [(pair? x) (and (pred (car x)) (list-of?-pred (cdr x)))]
                      [(null? x) #t]
                      [else #f]))])
      list-of?-pred))
  
  #;(define (datum? x)
    XXX)
  
)
