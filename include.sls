#!r6rs
(library (xitomatl include)
  (export
    include
    include/lexical-context
    include/resolve)
  (import 
    (rnrs)
    (for (xitomatl include compat) expand)
    (for (only (xitomatl file-system paths) path-join path?) expand)
    (for (only (xitomatl exceptions) error/conditions) expand))
  
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
         (let* ([fn (syntax->datum #'filename)]
                [datums 
                 (with-exception-handler
                   (lambda (ex)
                     (error/conditions 'include/lexical-context
                      "error while trying to include" (list fn)
                      (if (condition? ex) ex (make-irritants-condition (list ex)))))
                   (lambda ()
                     (call-with-input-file fn
                       (lambda (fip)
                         (let loop ([a '()])
                           (let ([x (read fip)])
                             (if (eof-object? x)
                               (reverse a)
                               (loop (cons x a)))))))))])
           (datum->syntax #'ctxt `(begin . ,datums)))])))
  
  (define-syntax include/resolve
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt (lib-path* ...) file-path)
         (for-all (lambda (s) (and (string? s) (positive? (string-length s)))) 
                  (syntax->datum #'(file-path lib-path* ...)))
         (let ([p (apply path-join (append (map syntax->datum #'(lib-path* ...))
                                           (list (syntax->datum #'file-path))))]
               [sp (search-paths)])
           (let loop ([search sp])
             (if (null? search)
               (error 'include/resolve "cannot find file in search paths" p sp)
               (let ([full (path-join (car search) p)])
                 (if (file-exists? full)
                   #`(include/lexical-context ctxt #,full)
                   (loop (cdr search)))))))])))
)
