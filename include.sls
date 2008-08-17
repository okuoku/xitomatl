#!r6rs
(library (xitomatl include)
  (export
    include
    include/lexical-context
    include/resolve)
  (import 
    (rnrs)
    (for (xitomatl include compat) expand)
    (for (xitomatl file-system paths) expand)
    (for (xitomatl conditions) expand))
  
  (define-syntax include
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt filename)
         #'(include/lexical-context ctxt filename)])))
  
  (define-syntax include/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt filename)
         (and (identifier? #'ctxt)
              (or (path? (syntax->datum #'filename))
                  (syntax-violation #f "not a path" stx #'filename)))
         (let ([fn (syntax->datum #'filename)])
           (with-exception-handler
             (lambda (ex)
               (error/conditions 'include/lexical-context
                 "error while trying to include" (list fn)
                 (if (condition? ex) ex (make-irritants-condition (list ex)))))
             (lambda ()
               (call-with-input-file fn
                 (lambda (fip)
                   (let loop ([x (read fip)] [a '()])
                     (if (eof-object? x)
                       (datum->syntax #'ctxt `(begin . ,(reverse a)))
                       (loop (read fip) (cons x a)))))))))])))
  
  (define-syntax include/resolve
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt (lib-path* ...) file-path)
         (for-all (lambda (s) (and (string? s) (positive? (string-length s)))) 
                  (syntax->datum #'(file-path lib-path* ...)))
         (let ([lp* (map syntax->datum #'(lib-path* ...))]
               [fp (syntax->datum #'file-path)])
           (let loop ([search (search-paths)])
             (if (null? search)
               (error 'include/resolve "cannot find file in search paths"
                      (apply path-join (append lp* (list fp)))
                      (search-paths))
               (let ([full (apply path-join (car search) (append lp* (list fp)))])
                 (if (file-exists? full)
                   #`(include/lexical-context ctxt #,full)
                   (loop (cdr search)))))))])))
)
