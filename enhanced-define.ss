;;; Provides define-values, 
;;; define* which does procedure currying and unspecified values,
;;; and define-syntax* which does procedure style

(library (enhanced-define)
  
  (export 
    define-values
    define*
    define-syntax*)
  
  (import 
    (except (rnrs) call-with-string-output-port)
    (only (ikarus) gensym))
  
  ;;; Until Ikarus implements this.
  (define-syntax call-with-string-output-port
    (syntax-rules ()
      [(_ proc)
       (let-values ([(sop get) (open-string-output-port)])
         (proc sop)
         (get))]))
  

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
                             (call-with-string-output-port 
                              (lambda (op)
                                (display "expected " op)
                                (display idlen op)
                                (display " values, received " op)
                                (display (length vl) op)
                                (display "values") op))
                             vl))
                    vl)))
               (define id* (let ([v (car g)]) (set! g (cdr g)) v))
               ...))])))
  

  (define-syntax define*
    (syntax-rules ()
      [(d ((maybe-list . f1) . f2) expr expr* ...)
       (d (maybe-list . f1)
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
