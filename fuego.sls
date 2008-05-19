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
    fuego-object? make-key key?
    root-object send #;resend
    ; Distinct key values for root-object's slots 
    :clone :unknown :already-exists :has? :keys
    :add-method! :add-parent! :add-value! :delete! 
    ; Convenient syntaxes    
    object define-values
    ; Exception condition
    &fuego? &fuego-object)  
  (import
    (rnrs)
    (only (xitomatl define extras) define/? define-values)
    (only (xitomatl conditions) assertion-violation/conditions)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand))
  
  ;-----------------------------------------------------------------------------    
  
  (define-record-type fuego-object
    (opaque #t) (sealed #t)
    (fields (mutable slots) (mutable parents)))
    
  (define-record-type key (fields name))
  
  ;-----------------------------------------------------------------------------    
  
  (define-condition-type &fuego &condition 
    make-&fuego &fuego?
    (object &fuego-object))
  
  (define (AV/F obj msg . irrts)
    (assertion-violation/conditions '(library (xitomatl fuego))
                                    msg irrts (make-&fuego obj)))
  
  ;-----------------------------------------------------------------------------  
  
  (define/? (send [obj fuego-object?] key . args)
    (find-key/handle obj obj key args #f #f))
  
  (define (resender receiver method-owner)
    (lambda (key . args)
      (let loop ([parents (fuego-object-parents method-owner)])
        (if (null? parents)
          (find-key/handle method-owner method-owner :unknown (cons key args)
            (lambda (ign) (AV/F method-owner "unknown key" key)) #f)
          (find-key/handle receiver (cdar parents) key args loop (cdr parents))))))

  (define (find-key/handle receiver search key args child-loop child-parents)
    ; Search obj's immediate slots
    ; If not found, search parents, depth-first
    (let ([found (assq key (fuego-object-slots search))])
      (if found
        (apply (cdr found) receiver (resender receiver search) args)
        (let loop ([parents (fuego-object-parents search)])
          (if (null? parents)
            (if child-loop
              (child-loop child-parents)
              (find-key/handle receiver receiver :unknown (cons key args)
                (lambda (ign) (AV/F receiver "unknown key" key)) #f))
            (find-key/handle receiver (cdar parents) key args loop (cdr parents)))))))

  ;-----------------------------------------------------------------------------
  
  (define root-clone
    (case-lambda
      [(self resend) (root-clone self resend (make-key 'cloned-parent))]
      [(self resend pk)
       (let ([o (make-fuego-object '() '())])
         (root-add-parent! o 'no-resend pk self)
         o)]))
  
  (define (root-delete! self resend key)
    (fuego-object-slots-set! self
      (remp (lambda (s) (eq? (car s) key)) 
            (fuego-object-slots self)))
    (fuego-object-parents-set! self
      (remp (lambda (pp) (eq? (car pp) key)) 
            (fuego-object-parents self))))  
  
  (define (root-has? self resend key)
    (if (assq key (fuego-object-slots self)) #t #f))

  (define/? (root-add-method! self resend key [proc procedure?])
    (if (root-has? self 'no-resend key)
      (send self :already-exists key proc)
      (fuego-object-slots-set! self
        (cons (cons key proc) (fuego-object-slots self)))))
  
  (define root-add-value! 
    (case-lambda
      [(self resend key val) (root-add-value! self resend key val #f)]
      [(self resend key val mutable)
       (define (ina s args) 
         (AV/F s "value method called with invalid number of arguments" (length args)))
       (root-add-method! self 'no-resend key
         (if mutable
           (case-lambda 
             [(s r) val] 
             [(s r n) (if (eq? s self)
                        (set! val n)
                        (root-add-value! s 'no-resend key n mutable))]
             [(s r . args) (ina s args)])
           (case-lambda 
             [(s r) val]
             [(s r n) (AV/F s "immutable value" key n)]
             [(s r . args) (ina s args)])))]))
  
  (define/? (root-add-parent! self resend key [obj fuego-object?])
    (let detect ([parents (fuego-object-parents obj)])
      (for-each (lambda (p) 
                  (if (eq? p self)
                    (AV/F self "parent cycle" obj)
                    (detect (fuego-object-parents p))))
                (map cdr parents)))
    (root-add-value! self 'no-resend key obj)
    (fuego-object-parents-set! self
      (append (fuego-object-parents self) (list (cons key obj)))))
    
  (define (root-keys self resend)
    (map car (fuego-object-slots self)))
  
  (define (root-unknown self resend key . vals)
    (AV/F self "unknown key" key))  
  
  (define (root-already-exists self resend key val)
    (AV/F self "slot already exists" key))  
  
  ;-----------------------------------------------------------------------------    
  
  (define-syntax define-root-keys
    (syntax-rules ()
      [(_ identifier ...)
       (begin (define identifier (make-key 'identifier)) ...)]))

  ; Distinct values used as keys used to access standard slots.
  ; Distinct values are used so access to any one of these slots can be 
  ; prevented by not supplying the corresponding value (capability-security).
  (define-root-keys :clone :unknown :already-exists :keys :has? 
                    :add-method! :add-parent! :add-value! :delete!)
  
  (define root-object
    (make-fuego-object 
     (list (cons :clone root-clone)
           (cons :unknown root-unknown)
           (cons :already-exists root-already-exists)
           (cons :keys root-keys)
           (cons :has? root-has?)
           (cons :add-method! root-add-method!)
           (cons :add-parent! root-add-parent!)
           (cons :add-value! root-add-value!)
           (cons :delete! root-delete!))
     '()))
  
  (define-syntax object
    (lambda (stx)
      (syntax-case stx ()
        [(kw body ...)
         (with-syntax ([method (datum->syntax #'kw 'method)]
                       [value (datum->syntax #'kw 'value)]
                       [parent (datum->syntax #'kw 'parent)])
           #'(let ([o (make-fuego-object '() '())]
                   [has-parent #f]
                   [keys '()])
               (define-syntax method
                 (lambda (stx)
                   (syntax-case stx (unquote)
                     [(_ (mn s r . a) b0 b (... ...))
                      #'(method mn (lambda (s r . a) b0 b (... ...)))]
                     [(_ (unquote mnk) expr) 
                      #'(root-add-method! o 'no-resend mnk expr)]
                     [(_ mn expr)
                      (syntax-case #'mn (quote)
                        [(quote x) (identifier? #'x)]
                        [x (identifier? #'x)])
                      (with-syntax* ([mnk (gen-temp)]
                                     [(mnk-e add-mnk (... ...)) 
                                      (if (identifier? #'mn) 
                                        #'((make-key 'mn) (set! keys (cons mnk keys)))
                                        #'(mn))])
                        #'(let ([mnk mnk-e])
                            (method ,mnk expr)
                            add-mnk (... ...)))])))
               (define-syntax value
                 (lambda (stx)
                   (syntax-case stx (unquote)
                     [(_ vn v) #'(value vn v #f)]
                     [(_ (unquote vnk) v m)
                      #'(root-add-value! o 'no-resend vnk v m)]
                     [(_ vn v m)
                      (syntax-case #'vn (quote)
                        [(quote x) (identifier? #'x)]
                        [x (identifier? #'x)])
                      (with-syntax* ([vnk (gen-temp)]
                                     [(vnk-e add-vnk (... ...)) 
                                      (if (identifier? #'vn)
                                        #'((make-key 'vn) (set! keys (cons vnk keys)))
                                        #'(vn))])
                        #'(let ([vnk vnk-e])
                            (value ,vnk v m)
                            add-vnk (... ...)))])))
               (define-syntax parent
                 (lambda (stx)
                   (syntax-case stx (unquote)
                     [(_ p) 
                      #'(let ([pnk (make-key 'parent)]) 
                          (parent ,pnk p))]
                     [(_ (unquote pnk) p)
                      #'(begin (root-add-parent! o 'no-resend pnk p)
                               (set! has-parent #t))]
                     [(_ pn p)
                      (syntax-case #'pn (quote)
                        [(quote x) (identifier? #'x)]
                        [x (identifier? #'x)])
                      (with-syntax* ([pnk (gen-temp)]
                                     [(pnk-e add-pnk (... ...)) 
                                      (if (identifier? #'pn)
                                        #'((make-key 'pn) (set! keys (cons pnk keys)))
                                        #'(pn))])
                        #'(let ([pnk pnk-e]) 
                            (parent ,pnk p)
                            add-pnk (... ...)))])))
               body ...
               (unless has-parent
                 (root-add-parent! o 'no-resend (make-key 'cloned-parent) root-object))
               (apply values o (reverse keys))))])))

)
