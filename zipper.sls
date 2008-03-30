#!r6rs
; Taken from Oleg's
; http://okmij.org/ftp/Scheme/zipper-in-scheme.txt

(library (xitomatl zipper)
  (export
    zipper? zipper-thing zipper-cont
    zip-iterate
    zip-finish 
    locate-nth-node
    map/left-right/preserving 
    depth-first 
    zip-iterate/df 
    locate-nth-node/df)
  (import 
    (rnrs)
    (only (xitomatl delimited-control) shift reset))
  
  
  (define-record-type zipper 
    (fields (immutable thing) 
            (immutable cont)))
  
  (define (zip-iterate iterate)
    (lambda (things)
      (reset (iterate (lambda (thing) (shift cont (make-zipper thing cont))) things))))
  
  (define (zip-finish zipper)
    (if (zipper? zipper) 
        (zip-finish ((zipper-cont zipper) (zipper-thing zipper)))
        zipper))
  
  (define (locate-nth-node iterate)
    (lambda (n tree)
      (define zt (zip-iterate iterate))
      (do ((i 0 (+ 1 i))
           (cursor (zt tree) ((zipper-cont cursor) #f)))
        ((and (= i n)
              (if (zipper? cursor) #t (error 'locate-nth-node "too few nodes")))
         cursor))))
  
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
  
  (define (depth-first handle tree)
    (cond
      [(handle tree) => (lambda (new-tree) new-tree)]
      ; the node was not handled -- descend
      [(null? tree) tree]
      [(not (pair? tree)) tree] ; an atom
      [else
       (let ([t (map/left-right/preserving (lambda (kid) (depth-first handle kid)) tree)])
         (if (eq? t tree) tree t))]))
  
  (define zip-iterate/df (zip-iterate depth-first))
  
  (define locate-nth-node/df (locate-nth-node depth-first))
  
)
