; Fuego -- A prototype-based object system supporting capability-based security.
;          By Derick Eddington.
;          Inspired by Prometheus by Jorgen Schaefer.

; TODO: learn about struct inspectors and think through the implications for security of information encapsulation
;       w/r/t struct inspecting and struct printing

; TODO: no possible exceptions should reveal info about underlying implementation.  I.e. arity errors on standard
;       methods should not cause an exception which reveals the name of the underlying.
;       For _real_ capability-security, this needs to be thouroughly analyzed and tamed

(library (fuego)
  
  (export fuego-root-object send fuego-object? make-fuego-object
          ; Distinct key values for fuego-root-object's slots 
          :clone :unknown-key :slot-already-exists :applied-without-key
          :has-parent-slot? :has-value-slot? :has-method-slot?
          :has-slot? :slots-list :delete-slot! 
          :delete-parent-slot! :delete-value-slot! :delete-method-slot!
          :add-parent-slot! :add-value-slot! :add-method-slot!
          :parent-slots-list :value-slots-list :method-slots-list
          ; Convenient syntaxes
          new-distinct #;define-object
          ; Exception conditions
          &fuego-error? &unknown-key? &slot-already-exists? &missing-key?)
  
  (import 
    (rnrs)
    (rnrs mutable-pairs)
    (only (ikarus) gensym))
  
  
  (define (alist-cons key datum alist) (cons (cons key datum) alist))
  
  (define alist-delete
    (case-lambda
      [(key alist)
       (alist-delete key alist equal?)]
      [(key alist f-eq?)
       (filter (lambda (p) (not (f-eq? (car p) key))) alist)]))
  
  ;-----------------------------------------------------------------------------  
  
  (define-record-type fuego-object
    (opaque #t) (sealed #t)
    (fields (mutable parent-slots)
            (mutable value-slots)
            (mutable method-slots)))
  
  (define (slots-delete key slots)
    (alist-delete key slots eq?))
  
  (define (slots-add! key value object accessor mutator!)
    (mutator!
      object
      (alist-cons key value 
                  (slots-delete key (accessor object)))))
  
  ;-----------------------------------------------------------------------------  
  
  (define (send self key . args)
    (find-key/handle self self key args #f))
  
  (define (send* self key . args)
    (find-key/handle self self key args
                     (lambda () (when (null? (fuego-object-parent-slots self))
                                  (error/fuego self "empty parent slots")))))
  
  
  (define (resend/parent receiver search)
    (lambda (key . args)
      (parents-handler receiver search key args #f)))
  
  
  (define (parents-handler receiver search key args not-found-thunk)
    (let loop ([parents (fuego-object-parent-slots search)])
      (if (null? parents)
        (if not-found-thunk
          (not-found-thunk)
          (send* receiver :unknown-key key args))
        (find-key/handle receiver (cdar parents) key args 
                         (lambda () (loop (cdr parents)))))))
  

  (define (find-key/handle receiver search key args not-found-thunk)
    ; Search self's immediate slots
    ; If not found, recursively search parents
    (let ([found (assq key (fuego-object-method-slots search))])
      (if found
        (apply (cdr found) receiver (resend/parent receiver search) args)
        (begin 
          (set! found (assq key (fuego-object-value-slots search)))
          (if found
            (cadr found)
            (begin
              (set! found (assq key (fuego-object-parent-slots search)))
              (if found 
                (cdr found)
                (parents-handler receiver search key args not-found-thunk))))))))
  
  ;-----------------------------------------------------------------------------    
  
  (define-condition-type &fuego-error &error 
    make-&fuego-error &fuego-error?
    (self &fuego-error-self))

  (define-condition-type &unknown-key &fuego-error
    make-&unknown-key &unknown-key?
    (key &unknown-key-key))
  
  (define-condition-type &slot-already-exists &fuego-error
    make-&slot-already-exists &slot-already-exists?
    (slot &slot-already-exists-slot))
  
  (define-condition-type &missing-key &fuego-error
    make-&missing-key &missing-key?)
  
  (define-syntax error/fuego
    (syntax-rules ()
      [(_ self error-str)
       (raise (condition (make-&fuego-error self)
                         (make-message-condition error-str)))]))
  
  (define-syntax error/unknown-key
    (syntax-rules ()
      [(_ self key)
       (raise (condition (make-&unknown-key self key)))]))
  
  (define-syntax error/slot-already-exists
    (syntax-rules ()
      [(_ self key)
       (raise (condition (make-&slot-already-exists self key)))]))
  
  (define-syntax error/missing-key
    (syntax-rules ()
      [(_ self)
       (raise (condition (make-&missing-key self)))]))

  ;-----------------------------------------------------------------------------
  
  ; Returns two values: a new Fuego object with the cloned object as a parent and as its only slot,
  ; a distinct value to be used as the key to access this parent slot.
  (define (standard-clone self resend)
    (make-fuego-object 
      (list (cons (new-distinct :parent) self)) ; parent-slots
      '()                                       ; value-slots
      '()))                                     ; method-slots
  
  
  (define (standard-unknown-key self resend key args)
    (error/unknown-key self key))
  
  
  (define (standard-slot-already-exists self resend key value type)
    (error/slot-already-exists self key))
  
  
  (define (standard-applied-without-key self resend)
    (error/missing-key self))
  
  
  (define (standard-delete-parent-slot! self resend key)
    (fuego-object-parent-slots-set! self 
      (slots-delete key (fuego-object-parent-slots self))))
  
  (define (standard-delete-value-slot! self resend key)
    (define value-slots (fuego-object-value-slots self))
    (cond [(assq key value-slots)
           =>
           (lambda (pair)
             (when (pair? (cddr pair))
               (standard-delete-method-slot! self #f (caddr pair))))])
    (fuego-object-value-slots-set! self 
      (slots-delete key value-slots)))
  
  (define (standard-delete-method-slot! self resend key)
    (fuego-object-method-slots-set! self 
      (slots-delete key (fuego-object-method-slots self))))
  
  (define (standard-delete-slot! self resend key)
    (send self :delete-parent-slot! key)
    (send self :delete-value-slot! key)
    (send self :delete-method-slot! key))
  
  
  (define (standard-has-parent-slot? self resend key)
    (if (assq key (fuego-object-parent-slots self)) #t #f))
  
  (define (standard-has-value-slot? self resend key)
    (if (assq key (fuego-object-value-slots self)) #t #f))
  
  (define (standard-has-method-slot? self resend key)
    (if (assq key (fuego-object-method-slots self)) #t #f))
  
  (define (standard-has-slot? self resend key)
    (if (or (send self :has-parent-slot? key)
            (send self :has-value-slot? key)
            (send self :has-method-slot? key))
      #t
      #f))
  
  
  (define-syntax with-duplicate-handling
    (syntax-rules ()
      [(_ (self key value type) exprs ...)
       (if (send self :has-slot? key)
         (send self :slot-already-exists key value type)
         (begin exprs ...))]))
  
  
  (define (standard-add-parent-slot! self resend key value)
    (with-duplicate-handling (self key value 'parent)
      (unless (fuego-object? value)
        (error/fuego self "not a fuego-object"))
      (slots-add! key value self fuego-object-parent-slots fuego-object-parent-slots-set!)))
  
  
  (define standard-add-value-slot!
    (case-lambda
      [(self resend/parent key value)
       (with-duplicate-handling (self key value 'value)
         (slots-add! key (list value) self 
                     fuego-object-value-slots fuego-object-value-slots-set!))]
      [(self/set resend/parent key value set-key)
       (with-duplicate-handling (self/set key value 'value)
         (let ([value-info (list value set-key)])
           (slots-add! key value-info self/set 
                       fuego-object-value-slots fuego-object-value-slots-set!)
           (standard-add-method-slot! self/set #f set-key
             (lambda (self resend new-value) 
               (if (eq? self self/set)
                 (set-car! value-info new-value)
                 (slots-add! key (list new-value) self 
                             fuego-object-value-slots fuego-object-value-slots-set!))))))]))
  
  
  (define (standard-add-method-slot! self resend key proc)
    (with-duplicate-handling (self key proc 'method)
      (unless (procedure? proc)
        (error/fuego self "not a procedure"))
      (slots-add! key proc self fuego-object-method-slots fuego-object-method-slots-set!)))
  
  
  (define (standard-parent-slots-list self resend)
    (map car (fuego-object-parent-slots self))) 
  
  (define (standard-value-slots-list self resend)
    (map car (fuego-object-value-slots self))) 
  
  (define (standard-method-slots-list self resend)
    (map car (fuego-object-method-slots self)))
  
  (define (standard-slots-list self resend)
    (append (send self :parent-slots-list)
            (send self :value-slots-list)
            (send self :method-slots-list)))
  
  ;-----------------------------------------------------------------------------    
  ; new-distinct doesn't have to use gensym; it can use anything which will yield
  ; distinct values which are not eq? and will not be eq? to any other value.
  
  (define-syntax new-distinct 
    (syntax-rules () 
      [(_ identifier) 
       (gensym 'identifier)]))
  
  (define-syntax define-distincts
    (syntax-rules ()
      [(_ identifier ...)
       (begin (define identifier (new-distinct identifier)) ...)]))

  ; Distinct values used as keys used to access standard slots.
  ; Distinct values are used so access to any one of these slots can be 
  ; prevented by not supplying the corresponding value (capability-security).
  (define-distincts :clone :unknown-key :slot-already-exists :applied-without-key
                    :has-parent-slot? :has-value-slot? :has-method-slot? 
                    :has-slot? :slots-list :delete-slot!
                    :delete-parent-slot! :delete-value-slot! :delete-method-slot!
                    :add-parent-slot! :add-value-slot! :add-method-slot!
                    :parent-slots-list :value-slots-list :method-slots-list)
  
  ;-----------------------------------------------------------------------------  
  
  (define fuego-root-object
    (make-fuego-object '()  ; parent-slots
                       '()  ; value-slots
                       ; method-slots
                       (list (cons :clone                    standard-clone)
                             (cons :unknown-key              standard-unknown-key)
                             (cons :slot-already-exists      standard-slot-already-exists)
                             (cons :applied-without-key      standard-applied-without-key)
                             (cons :delete-slot!             standard-delete-slot!)
                             (cons :delete-parent-slot!      standard-delete-parent-slot!)
                             (cons :delete-value-slot!       standard-delete-value-slot!)
                             (cons :delete-method-slot!      standard-delete-method-slot!)
                             (cons :has-slot?                standard-has-slot?)
                             (cons :has-parent-slot?         standard-has-parent-slot?)
                             (cons :has-value-slot?          standard-has-value-slot?)
                             (cons :has-method-slot?         standard-has-method-slot?)
                             (cons :add-parent-slot!         standard-add-parent-slot!)
                             (cons :add-value-slot!          standard-add-value-slot!)
                             (cons :add-method-slot!         standard-add-method-slot!)
                             (cons :parent-slots-list        standard-parent-slots-list) 
                             (cons :value-slots-list         standard-value-slots-list)
                             (cons :method-slots-list        standard-method-slots-list)
                             (cons :slots-list               standard-slots-list)) ))
  
)
