#!r6rs
(library (xitomatl queue)  
  (export 
    queue?
    make-empty-queue
    enqueue!
    dequeue!
    queue-length
    queue-empty?
    queue->list
    queue->list/reset
    check-queue)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (xitomatl define extras) define/? define/AV))

  (define-record-type queue 
    (fields (mutable head) (mutable end) #;sema))
  
  (define (make-empty-queue)
    (make-queue '() '() #;(make-semaphore 1)))
  
  (define/? (enqueue! [q queue?] e)
    (let ([el (cons e '())]
          #;[sema (queue-sema q)])
      #;(semaphore-wait sema)
      (let ([qe (queue-end q)])
        (unless (null? qe) (set-cdr! qe el))
        (queue-end-set! q el)
        (when (null? (queue-head q)) (queue-head-set! q el)))
      #;(semaphore-post sema)))
  
  (define/? (dequeue! [q queue?])
    (let (#;[sema (queue-sema q)])
      #;(semaphore-wait sema)
      (let ([h (queue-head q)])
        (when (null? h) (assertion-violation 'dequeue! "empty queue"))
        (let ([rest (cdr h)])
          (queue-head-set! q rest)
          (when (null? rest) (queue-end-set! q '())))
        #;(semaphore-post sema)
        (car h))))
  
  (define/? (queue-length [q queue?])
    (let (#;[sema (queue-sema q)])
      #;(semaphore-wait sema)
      (let ([r (length (queue-head q))])        
       #;(semaphore-post sema)
        r)))
  
  (define/? (queue-empty? [q queue?])
    (= 0 (queue-length q)))
  
  (define/? (queue->list [q queue?])
    (let (#;[sema (queue-sema q)])
      #;(semaphore-wait sema)
      (let ([r (apply list (queue-head q))])
        #;(semaphore-post sema)
        r)))
  
  (define/? (queue->list/reset [q queue?])
    (let (#;[sema (queue-sema q)])
      #;(semaphore-wait sema)
      (let ([h (queue-head q)])
        (queue-head-set! q '())
        (queue-end-set! q '())
        #;(semaphore-post sema)
        h)))
  
  (define/AV (check-queue q)
    (let (#;[sema (queue-sema q)])
      #;(semaphore-wait sema)
      (let ([head (queue-head q)]
            [end (queue-end q)])
        (if (null? head)
          (unless (null? end)
            (AV "head is null but end is not"))
          (unless (eq? end (list-tail head (- (length head) 1)))
            (AV "last pair of head is not end")))
        (if (null? end)
          (unless (null? head)
            (AV "end is null but head is not"))
          ; We know head is not null, therefore previous if block 
          ; will have checked that last pair of head is end
          ))
      #;(semaphore-post sema)))
)
