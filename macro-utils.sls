#!r6rs
(library (xitomatl macro-utils)
  (export
    gen-temp #;gensym
    duplicate-id unique-ids? unique-ids?/raise
    formals-ok?
    identifier-append
    unwrap syntax->list #;syntax-map
    with-syntax*)
  (import
    (rnrs))
  
  (define (gen-temp)
    (with-syntax ([(t) (generate-temporaries '(1))])
      #'t))
  
  #;(define (gensym) (syntax->datum (gen-temp)))
  
  (define (duplicate-id ls)
    (if (null? ls)
      #f
      (or
        (let loop ([x (car ls)] [rest (cdr ls)])
          (if (null? rest)
            #f
            (if (bound-identifier=? x (car rest))
              x
              (loop x (cdr rest)))))
        (duplicate-id (cdr ls)))))
  
  (define (unique-ids? ls)
    (not (duplicate-id ls)))
  
  (define unique-ids?/raise
    (case-lambda
      [(ids stx msg)
       (let ([dup (duplicate-id ids)])
         (if dup
           (syntax-violation #f msg stx dup)
           #t))]
      [(ids stx)
       (unique-ids?/raise ids stx "duplicate identifier")]))
  
  (define (formals-ok? frmls-stx orig-stx)
    (syntax-case frmls-stx ()
      [(arg* ... . rest)
       (and (or (null? (syntax->datum #'rest))
                (identifier? #'rest)
                (syntax-violation #f "not an identifier" orig-stx #'rest))
            (for-all (lambda (id)
                       (or (identifier? id)
                           (syntax-violation #f "not an identifier" orig-stx id)))
                     #'(arg* ...))
            (unique-ids?/raise 
              (append
                #'(arg* ...)
                (if (identifier? #'rest) (list #'rest) '())) 
              orig-stx))]))
  
  (define (unwrap stx)
    (guard (ex [(syntax-violation? ex) 
                (assertion-violation 'unwrap "invalid argument" stx)])
      (let uw ([stx stx])
        (syntax-case stx ()
          [(x . r) (cons (uw #'x) (uw #'r))]
          [#(x ...) (apply vector (uw #'(x ...)))]
          [x (identifier? #'x) #'x]
          [x (syntax->datum #'x)]))))
  
  (define (identifier-append ctxt . ids)
    (define who 'identifier-append)
    (unless (identifier? ctxt) (assertion-violation who "not an identifier" ctxt))    
    (let ([rs
           (apply string-append
             (map 
               (lambda (id)
                 (cond [(identifier? id) (symbol->string (syntax->datum id))]
                       [(symbol? id) (symbol->string id)]
                       [(string? id) id]
                       [else
                        (assertion-violation who "not an identifier, symbol, or string" id)]))
               ids))])
      (unless (positive? (string-length rs))
        (assertion-violation who "result length zero" rs))
      (datum->syntax ctxt (string->symbol rs))))
  
  #;(define-syntax define-syntax-iterate
    (lambda (stx)
      (syntax-case stx ()
        [(_ name iter) 
         (for-all identifier? (list #'name #'iter)) 
         #'(define (name f ls)
             (syntax-case ls ()
               [(ls (... ...)) (iter f #'(ls (... ...)))]
               [_ (assertion-violation 'name "not a syntax list" ls)]))])))
  
  #;(define-syntax-iterate syntax-map map)
     
  (define (syntax->list ls)
    (syntax-case ls ()
      [(ls ...) #'(ls ...)]
      [_ (assertion-violation 'syntax->list "not a syntax list" ls)]))
  
  (define-syntax with-syntax*
    (syntax-rules ()
      [(_ (pc0 pc1 pc* ...) b b* ...)
       (with-syntax (pc0)
         (with-syntax* (pc1 pc* ...) b b* ...))]
      [(_ pc b b* ...)
       (with-syntax pc b b* ...)]))
  
)
