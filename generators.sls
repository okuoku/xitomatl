#!r6rs
(library (xitomatl generators)
  (export
    make-generator
    define-generator
    &generator-finished?)
  (import
    (rnrs))
  
  ;; Modified from Will Farr's generators example:
  ;; http://wmfarr.blogspot.com/2006/08/one-more-example-of-python-generators.html
  
  ;; NOTE: A generator is not intended to be used concurrently by threads/engines,
  ;;       and is not thread/engine safe.
  
  (define-condition-type &generator-finished &condition
    make-&generator-finished &generator-finished?)
  
  (define (make-generator proc)
    (letrec ([resume (lambda ()
                       (proc yield)
                       (raise (make-&generator-finished)))]
             [return #f]
             [yield (lambda args
                      (call/cc
                        (lambda (k)
                          (set! resume k)
                          (apply return args))))])
      (lambda ()
        (call/cc
          (lambda (k)
            (set! return k)
            (resume))))))
  
  (define-syntax define-generator
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt (name . frmls) b0 b ...)
         (with-syntax ([yield (datum->syntax #'ctxt 'yield)])
           #'(define (name . frmls)
               (make-generator
                 (lambda (yield)
                   b0 b ...))))])))

)
