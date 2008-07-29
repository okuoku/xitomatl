#!r6rs
(library (xitomatl tests irregex test)
  (export
    test test-assert test-error test-group
    test-begin test-end test-exit)
  (import
    (rnrs)
    (xitomatl srfi lightweight-testing))
  
  (define-syntax test 
    (syntax-rules ()
      [(_ name expected expr)
       (test expected expr)]
      [(_ expected expr)
       (check expr => expected)]))
  
  (define-syntax test-assert
    (syntax-rules ()
      [(_ name expr)
       (test-assert expr)]
      [(_ expr)
       (check (and expr #t) => #t)]))
  
  (define-syntax test-error
    (syntax-rules ()
      [(_ name expr)
       (test-error expr)]
      [(_ expr)
       (check (guard (ex [(or (error? ex)
                              (assertion-violation? ex)) 
                          #t]
                         [else `(dont-know: ,ex)])
                expr
                '(succeeded: expr))
              => #t)]))
  
  (define-syntax test-group
    (syntax-rules () 
      [(_ name expr0 expr ...)
       (begin expr0 expr ...)]))
  
  (define-syntax test-begin
    (syntax-rules ()
      [(_)
       (begin)]))
  
  (define (test-end) (check-report))
  
  (define (test-exit n) 
    (if (check-passed? n)
      (exit)
      (exit #f)))
  
)
