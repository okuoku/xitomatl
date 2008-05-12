#!r6rs
; Fuego -- A prototype-based object system supporting capability-based security.
;          By Derick Eddington.
;          Inspired by Prometheus by Jorgen Schaefer.

; TODO: learn about struct inspectors and think through the implications for security of information encapsulation
;       w/r/t struct inspecting and struct printing

; TODO: no possible exceptions should reveal info about underlying implementation.  I.e. arity errors on standard
;       methods should not cause an exception which reveals the name of the underlying.
;       For _real_ capability-security, this needs to be thouroughly analyzed and tamed

(library (xitomatl fuego)  
  (export 
    root-object send fuego-object?
    ; Distinct key values for root-object's slots 
    :clone :unknown-key :slot-already-exists
    :has-parent-slot? :has-value-slot? :has-method-slot?
    :has-slot? :slots-keys :delete-slot! 
    :delete-parent-slot! :delete-value-slot! :delete-method-slot!
    :add-parent-slot! :add-value-slot! :add-method-slot!
    :parent-slots-keys :value-slots-keys :method-slots-keys
    ; Convenient syntaxes
    new-distinct define-distincts
    object define-values
    ; Exception conditions
    &fuego? &unknown-key? &slot-already-exists?)  
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (xitomatl define extras) define/? define-values)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand))
  
  ;-----------------------------------------------------------------------------    
  
  (define-record-type fuego-object
    (opaque #t) (sealed #t)
    (fields (mutable parent-slots)
            (mutable value-slots)
            (mutable method-slots)))
    
  (define (slots-delete key slots)
    (remp (lambda (p) (eq? (car p) key)) slots))
  
  ;-----------------------------------------------------------------------------    
  
  (define-condition-type &fuego &condition 
    make-&fuego &fuego?
    (obj &fuego-obj))

  (define-condition-type &unknown-key &fuego
    make-&unknown-key &unknown-key?
    (key &unknown-key-key))
  
  (define-condition-type &slot-already-exists &fuego
    make-&slot-already-exists &slot-already-exists?
    (slot &slot-already-exists-slot))
  
  (define (raise-&fuego obj str)
    (raise (condition (make-&fuego obj) (make-message-condition str))))
  
  (define (raise-&unknown-key obj key)
    (raise (condition (make-&unknown-key obj key))))
  
  (define (raise-&slot-already-exists obj key)
    (raise (condition (make-&slot-already-exists obj key))))
  
  ;-----------------------------------------------------------------------------  
  
  (define/? (send [obj fuego-object?] key . args)
    (find-key/handle obj obj key args #f))  

  (define (find-key/handle receiver search key args not-found-thunk)
    ; Search obj's immediate slots
    ; If not found, recursively search parents
    (let ([found (assq key (fuego-object-method-slots search))])
      (if found
        (apply (cdr found) receiver args)
        (begin
          (set! found (assq key (fuego-object-value-slots search)))
          (if found
            (cadr found)
            (let ([parents (fuego-object-parent-slots search)])
              (set! found (assq key parents))
              (if found 
                (cdr found)
                (let loop ([parents parents])
                  (if (null? parents)
                    (if not-found-thunk
                      (not-found-thunk)
                      (find-key/handle receiver search :unknown-key (list key args)
                                       (lambda () (when (null? parents)
                                                    (raise-&fuego search "empty parent slots")))))
                    (find-key/handle receiver (cdar parents) key args 
                                     (lambda () (loop (cdr parents)))))))))))))

  ;-----------------------------------------------------------------------------
  
  (define root-clone
    (case-lambda
      [(self) (root-clone self (new-distinct ':parent))]
      [(self pn)
       (make-fuego-object 
        (list (cons pn self)) ; parent-slots
        '()                   ; value-slots
        '())]))               ; method-slots  
  
  (define (root-unknown-key self key ignore)
    (raise-&unknown-key self key))  
  
  (define (root-slot-already-exists self key value type)
    (raise-&slot-already-exists self key))  
  
  (define (root-delete-parent-slot! self key)
    (fuego-object-parent-slots-set! self 
      (slots-delete key (fuego-object-parent-slots self))))
  
  (define (root-delete-value-slot! self key)
    (let ([value-slots (fuego-object-value-slots self)])
      (cond [(assq key value-slots)
             => (lambda (pair)
                  (when (pair? (cddr pair))
                    (root-delete-method-slot! self (caddr pair))))])
      (fuego-object-value-slots-set! self (slots-delete key value-slots))))
  
  (define (root-delete-method-slot! self key)
    (fuego-object-method-slots-set! self 
      (slots-delete key (fuego-object-method-slots self))))
  
  (define (root-delete-slot! self key)
    (root-delete-parent-slot! self key)
    (root-delete-value-slot! self key)
    (root-delete-method-slot! self key))  
  
  (define (root-has-parent-slot? self key)
    (if (assq key (fuego-object-parent-slots self)) #t #f))
  
  (define (root-has-value-slot? self key)
    (if (assq key (fuego-object-value-slots self)) #t #f))
  
  (define (root-has-method-slot? self key)
    (if (assq key (fuego-object-method-slots self)) #t #f))
  
  (define (root-has-slot? self key)
    (or (root-has-parent-slot? self key)
        (root-has-value-slot? self key)
        (root-has-method-slot? self key)))
    
  (define/? (root-add-parent-slot! self key [value fuego-object?])
    (if (root-has-slot? self key)
      (send self :slot-already-exists key value 'parent)
      (fuego-object-parent-slots-set! self
        (cons (cons key value)
              (fuego-object-parent-slots self)))))  
  
  (define root-add-value-slot!
    (case-lambda
      [(self key value)
       (if (root-has-slot? self key)
         (send self :slot-already-exists key value 'value)
         (fuego-object-value-slots-set! self
           (cons (cons key (list value))
                 (fuego-object-value-slots self))))]
      [(self key value set-key)
       (if (root-has-slot? self key)
         (send self :slot-already-exists key value 'value)
         (let ([value-info (list value set-key)])
           (fuego-object-value-slots-set! self
             (cons (cons key value-info)
                   (fuego-object-value-slots self)))
           (root-add-method-slot! self set-key
             (lambda (s new-value) 
               (if (eq? s self)
                 (set-car! value-info new-value)
                 (let ([vs (fuego-object-value-slots s)])
                   (cond [(assq key vs)
                          => (lambda (p) (set-car! (cdr p) new-value))]
                         [else
                          (fuego-object-value-slots-set! s
                            (cons (cons key (list new-value)) vs))])))))))]))
    
  (define/? (root-add-method-slot! self key [proc procedure?])
    (if (root-has-slot? self key)
      (send self :slot-already-exists key proc 'method)
      (fuego-object-method-slots-set! self 
        (cons (cons key proc) (fuego-object-method-slots self)))))
    
  (define (root-parent-slots-keys self)
    (map car (fuego-object-parent-slots self))) 
  
  (define (root-value-slots-keys self)
    (map car (fuego-object-value-slots self))) 
  
  (define (root-method-slots-keys self)
    (map car (fuego-object-method-slots self)))
  
  (define (root-slots-keys self)
    (append (root-parent-slots-keys self)
            (root-value-slots-keys self)
            (root-method-slots-keys self)))
  
  ;-----------------------------------------------------------------------------    
  
  (define-record-type fuego-key (fields name))
  
  (define (new-distinct name) 
    ; A new distinct can use anything which will yield
    ; distinct values which are not eq? and will not be eq? to any other value.  
    (make-fuego-key name))
  
  (define-syntax define-distincts
    (syntax-rules ()
      [(_ identifier ...)
       (begin (define identifier (new-distinct 'identifier)) ...)]))

  ; Distinct values used as keys used to access standard slots.
  ; Distinct values are used so access to any one of these slots can be 
  ; prevented by not supplying the corresponding value (capability-security).
  (define-distincts :clone :unknown-key :slot-already-exists
                    :has-parent-slot? :has-value-slot? :has-method-slot? 
                    :has-slot? :slots-keys :delete-slot!
                    :delete-parent-slot! :delete-value-slot! :delete-method-slot!
                    :add-parent-slot! :add-value-slot! :add-method-slot!
                    :parent-slots-keys :value-slots-keys :method-slots-keys)
  
  ;-----------------------------------------------------------------------------  
  
  (define root-object
    (make-fuego-object '()  ; parent-slots
                       '()  ; value-slots
                       ; method-slots
                       (list (cons :clone                    root-clone)
                             (cons :unknown-key              root-unknown-key)
                             (cons :slot-already-exists      root-slot-already-exists)
                             (cons :delete-slot!             root-delete-slot!)
                             (cons :delete-parent-slot!      root-delete-parent-slot!)
                             (cons :delete-value-slot!       root-delete-value-slot!)
                             (cons :delete-method-slot!      root-delete-method-slot!)
                             (cons :has-slot?                root-has-slot?)
                             (cons :has-parent-slot?         root-has-parent-slot?)
                             (cons :has-value-slot?          root-has-value-slot?)
                             (cons :has-method-slot?         root-has-method-slot?)
                             (cons :add-parent-slot!         root-add-parent-slot!)
                             (cons :add-value-slot!          root-add-value-slot!)
                             (cons :add-method-slot!         root-add-method-slot!)
                             (cons :parent-slots-keys        root-parent-slots-keys) 
                             (cons :value-slots-keys         root-value-slots-keys)
                             (cons :method-slots-keys        root-method-slots-keys)
                             (cons :slots-keys               root-slots-keys)) ))
  
  (define-syntax object
    (lambda (stx)
      (syntax-case stx ()
        [(kw () body ...)
         #'(kw (root-object) body ...)]
        [(kw (main-parent) body ...)
         (with-syntax ([method (datum->syntax #'kw 'method)]
                       [value (datum->syntax #'kw 'value)]
                       [parent (datum->syntax #'kw 'parent)])
           #'(let ([o (send main-parent :clone)]
                   [keys '()])
               (define-syntax method
                 (lambda (stx)
                   (syntax-case stx ()
                     [(_ (mn s . ra) b0 b (... ...))
                      (and (syntax-case #'mn (quote)
                             [(quote x) (identifier? #'x)]
                             [x (identifier? #'x)])
                           (identifier? #'s))
                      (with-syntax* ([mnk (gen-temp)]
                                     [(mnk-e add-mnk (... ...)) 
                                      (if (identifier? #'mn) 
                                        #'((new-distinct 'mn) (set! keys (cons mnk keys)))
                                        #'(mn))])
                        #'(let ([mnk mnk-e])
                            (send o :add-method-slot! mnk 
                                  (lambda (s . ra) b0 b (... ...)))
                            add-mnk (... ...)))])))
               (define-syntax value
                 (lambda (stx)
                   (syntax-case stx ()
                     [(_ vn v sn (... ...))
                      (and (syntax-case #'vn (quote)
                             [(quote x) (identifier? #'x)]
                             [x (identifier? #'x)])
                           (or (zero? (length #'(sn (... ...))))
                               (and (= 1 (length #'(sn (... ...))))
                                    (syntax-case (car #'(sn (... ...))) (quote)
                                      [(quote x) (identifier? #'x)]
                                      [x (identifier? #'x)]))))
                      (with-syntax* ([vnk (gen-temp)]
                                     [(snk (... ...)) (generate-temporaries #'(sn (... ...)))]
                                     [(vnk-e add-vnk (... ...)) 
                                      (if (identifier? #'vn)
                                        #'((new-distinct 'vn) (set! keys (cons vnk keys)))
                                        #'(vn))]
                                     [((snk-e add-snk (... ...)) (... ...)) 
                                      (map (lambda (x y) 
                                             (if (identifier? x) 
                                               #`((new-distinct '#,x) (set! keys (cons #,y keys)))
                                               (list x))) 
                                           #'(sn (... ...))
                                           #'(snk (... ...)))])
                        #'(let ([vnk vnk-e] [snk snk-e] (... ...))
                            (send o :add-value-slot! vnk v snk (... ...))
                            add-vnk (... ...)
                            add-snk (... ...) (... ...)))])))
               (define-syntax parent
                 (lambda (stx)
                   (syntax-case stx ()
                     [(_ pn p)
                      (syntax-case #'pn (quote)
                        [(quote x) (identifier? #'x)]
                        [x (identifier? #'x)])
                      (with-syntax* ([pnk (gen-temp)]
                                     [(pnk-e add-pnk (... ...)) 
                                      (if (identifier? #'pn)
                                        #'((new-distinct 'pn) (set! keys (cons pnk keys)))
                                        #'(pn))])
                        #'(let ([pnk pnk-e]) 
                            (send o :add-parent-slot! pnk p)
                            add-pnk (... ...)))])))
               body ...
               (apply values o (reverse keys))))])))
  
)
