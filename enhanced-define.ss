;;; Provides define-values, 
;;; define* which does procedure currying and unspecified values,
;;; and define-syntax* which does procedure style

(library (enhanced-define)
  
  (export 
    define-values
    define*
    define-syntax*)
  
  (import 
    (rnrs)
    (only (ikarus) gensym))
  

  (define-syntax define-values
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id* ...) expr)
         (positive? (length (syntax->datum #'(id* ...))))
         (with-syntax ([g (gensym)]
                       [idlen (length (syntax->datum #'(id* ...)))])
           #'(begin 
               (define g 
                 (call-with-values 
                  (lambda () expr) 
                  (lambda vl 
                    (unless (= (length vl) idlen)
                      (apply error 'define-values
                             (string-append "expected "
                                            (number->string idlen)
                                            " values, received "
                                            (number->string (length vl))
                                            " values")
                             vl))
                    vl)))
               (define id* (let ([v (car g)]) (set! g (cdr g)) v))
               ...))])))
  

  (define-syntax define*
    (syntax-rules ()
      [(_ ((maybe-list . f1) . f2) expr expr* ...)
       (define* (maybe-list . f1)
         (lambda f2 expr expr* ...))]
      [(_ (name . formals) expr expr* ...)
       (define (name . formals) expr expr* ...)]
      [(_ name expr)
       (define name expr)]
      [(_ name)
       (define name (if #f #f))]))  

    
  (define-syntax define-syntax*
    (syntax-rules ()
      [(_ (name . args) expr expr* ...)
       (define-syntax name
         (lambda args expr expr* ...))]
      [(_ name expr)
       (define-syntax name expr)]))
  
)
