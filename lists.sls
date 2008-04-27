#!r6rs
(library (xitomatl lists)
  (export
    map/left-right/preserving
    rem-dups)
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

)
