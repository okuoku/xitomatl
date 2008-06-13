#!r6rs
(library (xitomatl alists)
  (export
    alist? pred-alist? equal-alist? eqv-alist? eq-alist?
    make-pred-alist make-equal-alist make-eqv-alist make-eq-alist
    alist-ref assp-ref assoc-ref assv-ref assq-ref
    alist-set! assp-replace assoc-replace assv-replace assq-replace
    alist-delete! assp-remove assoc-remove assv-remove assq-remove
    alist-update! assp-update assoc-update assv-update assq-update
    alist-copy ass-copy alist-clear!
    alist-keys ass-keys alist-entries ass-entries
    alist-contains? alist-size alist-equivalence-function alist-mutable?)
  (import
    (rnrs)
    (only (xitomatl define extras) define/? define/AV))
  
  ;;; This library API is designed to be compatible with (rnrs hashtables)
  ;;; so that alists can be swapped for hashtables and vice versa.
  
  ;;; The procedures starting with alist- operate on the alist record type
  ;;; defined in this library.  The procedures not starting with alist-
  ;;; operate on raw association lists and are functional (never mutate).
  
  (define-record-type alist 
    (fields (mutable al) m))
  (define (make-alist-subtype-protocol who)
    (lambda (p)
      (letrec ([f (case-lambda 
                    [() (f '() #t)]
                    [(al) (f al #t)]
                    [(al m) 
                     (unless (and (list? al) (for-all pair? al))
                       (assertion-violation who "not a list of pairs" al))
                     (unless (boolean? m)
                       (assertion-violation who "not a boolean" m))
                     ((p al m))])])
        f)))
  (define-record-type pred-alist (parent alist) 
    (protocol (make-alist-subtype-protocol 'make-pred-alist)))
  (define-record-type equal-alist (parent alist) 
    (protocol (make-alist-subtype-protocol 'make-equal-alist)))
  (define-record-type eqv-alist (parent alist) 
    (protocol (make-alist-subtype-protocol 'make-eqv-alist)))
  (define-record-type eq-alist (parent alist) 
    (protocol (make-alist-subtype-protocol 'make-eq-alist)))
  
  (define (make-dispatch asspf assocf assvf assqf)
    (lambda (a) 
      (cond [(eq-alist? a) assqf]
            [(equal-alist? a) assocf]
            [(pred-alist? a) asspf]
            [(eqv-alist? a) assvf])))
  
  (define (make-alist-al-set!/check-immutable who)
    (lambda (a al)
      (if (alist-m a)
        (alist-al-set! a al)
        (assertion-violation who "alist is immutable" a))))
  
  (define-syntax define-alist-proc
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt name (asspf assocf assvf assqf) expr) 
         (identifier? #'name)
         (with-syntax ([d (datum->syntax #'ctxt 'dispatch)]
                       [s (datum->syntax #'ctxt 'alist-al-set!/check-immutable)])
           #'(define/? name
               (let ([d (make-dispatch asspf assocf assvf assqf)]
                     [s (make-alist-al-set!/check-immutable 'name)])
                 expr)))])))
  
  ;--------------------------------------------------------------------------
  
  (define/? (alist-size [a alist?])
    (length (alist-al a)))
  
  ;--------------------------------------------------------------------------
  
  (define-syntax define-ass-ref
    (syntax-rules ()
      [(_ name a)
       (define/AV name 
         (case-lambda
           [(al key-or-pred default)
            (cond [(a key-or-pred al) => cdr]
                  [else default])]
           [(al key-or-pred)
            (cond [(a key-or-pred al) => cdr]
                  [else (AV "not found" key-or-pred)])]))]))
  
  (define-ass-ref assp-ref assp)
  (define-ass-ref assoc-ref assoc)
  (define-ass-ref assv-ref assv)
  (define-ass-ref assq-ref assq)
  
  (define-alist-proc alist-ref 
                     (assp-ref assoc-ref assv-ref assq-ref)
    (case-lambda/?
      [([a alist?] kop d)
       ((dispatch a) (alist-al a) kop d)]
      [([a alist?] kop) 
       ((dispatch a) (alist-al a) kop)]))
  
  ;--------------------------------------------------------------------------
  
  (define/AV (assp-replace al pred obj)
    ;;; Unlike the other ass-replace-ers, assp-replace must find an existing
    ;;; association so it can know what key to use.
    (let ([key #f] [found #f])
      (let ([keep (remp (lambda (p)
                         (let ([k (car p)])
                           (cond [(pred k) (set! key k) (set! found #t) #t]
                                 [else #f]))) 
                        al)])
        (unless found (AV "no key found"))
        (cons (cons key obj) keep))))
  
  (define-syntax define-ass-replace
    (syntax-rules ()
      [(_ name e)
       (define (name al key obj)
         (cons (cons key obj) 
               (remp (lambda (p) (e key (car p))) al)))]))
  
  (define-ass-replace assoc-replace equal?)
  (define-ass-replace assv-replace eqv?)
  (define-ass-replace assq-replace eq?)
  
  (define-alist-proc alist-set! 
                     (assp-replace assoc-replace assv-replace assq-replace)
    (lambda/? ([a alist?] kop o)
      (alist-al-set!/check-immutable a ((dispatch a) (alist-al a) kop o))))
  
  ;--------------------------------------------------------------------------
  
  (define (assp-remove al pred)
    (remp (lambda (p) (pred (car p))) al))
  
  (define-syntax define-ass-remove
    (syntax-rules ()
      [(_ name e)
       (define (name al key)
         (remp (lambda (p) (e key (car p))) al))]))
  
  (define-ass-remove assoc-remove equal?)
  (define-ass-remove assv-remove eqv?)
  (define-ass-remove assq-remove equal?)
  
  (define-alist-proc alist-delete!
                     (assp-remove assoc-remove assv-remove assq-remove)
    (lambda/? ([a alist?] kop) 
      (alist-al-set!/check-immutable a ((dispatch a) (alist-al a) kop))))
  
  ;--------------------------------------------------------------------------
  
  (define-alist-proc alist-contains? 
                     (assp assoc assv assq)
    (lambda/? ([a alist?] kop)
      (if ((dispatch a) kop (alist-al a)) #t #f)))
  
  ;--------------------------------------------------------------------------
  
  (define/AV assp-update 
    (case-lambda
      [(al pred proc)
       (let* ([found #f]
              [new (map (lambda (p)
                          (let ([k (car p)])
                            (cond [(pred k) (set! found #t) (cons k (proc (cdr p)))]
                                  [else p])))
                        al)])           
         (if found new (AV "no key found")))]
      [(al pred proc default) (assp-update al pred proc)]))
  
  (define-syntax define-ass-update
    (syntax-rules ()
      [(_ name e)
       (define (name al key proc default)
         (let* ([found #f]
                [new (map (lambda (p)
                            (let ([k (car p)])
                              (cond [(e key k) (set! found #t) (cons k (proc (cdr p)))]
                                    [else p])))
                          al)])           
           (if found new (cons (cons key (proc default)) al))))]))
  
  (define-ass-update assoc-update equal?)
  (define-ass-update assv-update eqv?)
  (define-ass-update assq-update eq?)
  
  (define-alist-proc alist-update!
                     (assp-update assoc-update assv-update assq-update)
    (lambda/? ([a alist?] kop p d)
      (alist-al-set!/check-immutable a ((dispatch a) (alist-al a) kop p d))))
  
  ;--------------------------------------------------------------------------

  (define (ass-copy al)
    (map (lambda (p) (cons (car p) (cdr p))) al))
  
  (define-alist-proc alist-copy 
                     (make-pred-alist make-equal-alist make-eqv-alist make-eq-alist)
    (case-lambda/?
      [(a) (alist-copy a #f)]
      [([a alist?] mutable)
       ((dispatch a) (ass-copy (alist-al a)) mutable)]))
  
  ;--------------------------------------------------------------------------
  
  (define/? alist-clear!
    (let ([alist-al-set!/check-immutable
           (make-alist-al-set!/check-immutable 'alist-clear!)]) 
      (lambda/? ([a alist?])
        (alist-al-set!/check-immutable a '()))))
  
  ;--------------------------------------------------------------------------
  
  (define (ass-keys al)
    (map car al))
  
  (define/? (alist-keys [a alist?])
    (list->vector (ass-keys (alist-al a))))
  
  ;--------------------------------------------------------------------------
  
  (define/AV (ass-entries al)
    (let loop ([keys '()] [vals '()] [al al])
      (cond [(pair? al) 
             (let ([p (car al)])
               (loop (cons (car p) keys) (cons (cdr p) vals) (cdr al)))]
            [(null? al) (values (reverse keys) (reverse vals))]
            [else (AV "not a proper list")])))
  
  (define/? (alist-entries [a alist?])
    (let-values ([(kl vl) (ass-entries (alist-al a))])
      (values (list->vector kl) (list->vector vl))))
  
  ;--------------------------------------------------------------------------

  (define-alist-proc alist-equivalence-function
                     ('pred equal? eqv? eq?)
    (lambda/? ([a alist?])
      (dispatch a)))
  
  (define/? (alist-mutable? [a alist?])
    (alist-m a))

)
