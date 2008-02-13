(library (indexes)
  (export
    enumerate-indexes)
  (import
    (rnrs))
  
  (define (enumerate-indexes l)
    (do ([i (- (length l) 1) (- i 1)]
         [indexes '() (cons i indexes)])
      [(= i -1)
       indexes]))
  
)
