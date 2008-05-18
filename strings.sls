#!r6rs
(library (xitomatl strings)
  (export
    string-intersperse
    string-split)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (xitomatl lists) intersperse))
  
  (define (string-intersperse sl ssep)
    (apply string-append (intersperse sl ssep)))
  
  (define string-split
    (case-lambda
      [(str) (string-split str "\t\n\r " #f)]
      [(str delim-strs) (string-split str delim-strs #f)]
      [(str delim-strs keep-empty?)
       (unless (string? str)
         (assertion-violation 'string-split "not a string" str))
       (unless (string? delim-strs)
         (assertion-violation 'string-split "not a string" delim-strs))
       (let ([strlen (string-length str)]
             [dellen (string-length delim-strs)] 
             [first #f])
         (define (add from to last)
           (let ([node (cons (substring str from to) '())])
             (if first
               (set-cdr! last node)
               (set! first node) ) 
             node))
         (let loop ([i 0] [last #f] [from 0])
           (cond [(fx>=? i strlen)
                  (when (or (fx>? i from) keep-empty?) (add from i last))
                  (or first '()) ]
                 [else
                  (let ([c (string-ref str i)])
                    (let scan ([j 0])
                      (cond [(fx>=? j dellen) (loop (fx+ i 1) last from)]
                            [(eq? c (string-ref delim-strs j))
                             (let ([i2 (fx+ i 1)])
                               (if (or (fx>? i from) keep-empty?)
                                 (loop i2 (add from i last) i2)
                                 (loop i2 last i2)))]
                            [else (scan (fx+ j 1))])))])))]))
)
