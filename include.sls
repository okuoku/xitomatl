#!r6rs
(library (xitomatl include)
  (export
    include
    include/lexical-context
    include/resolve)
  (import 
    (rnrs)
    (only (xitomatl common-unstandard) format)
    (for (only (xitomatl file-system paths) dir-sep-str) expand)
    (for (xitomatl include compat) expand))
  
  (define-syntax include
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt filename)
         #'(include/lexical-context ctxt filename)])))
  
  (define-syntax include/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt filename)
         (string? (syntax->datum #'filename))
         (call-with-input-file (syntax->datum #'filename)
           (lambda (fip)
             (let loop ([x (read fip)] [a '()])
               (if (eof-object? x)
                 (datum->syntax #'ctxt `(begin . ,(reverse a)))
                 (loop (read fip) (cons x a))))))])))
  
  (define-syntax include/resolve
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt (lib-path* ...) file-path)
         (for-all (lambda (s) (and (string? s) (positive? (string-length s)))) 
                  (syntax->datum #'(file-path lib-path* ...)))
         (let ([lp*/sep (apply string-append 
                               (map (lambda (ps) (string-append ps dir-sep-str)) 
                                    (syntax->datum #'(lib-path* ...))))]
               [fp (syntax->datum #'file-path)])
           (let loop ([search (search-paths)])
             (if (null? search)
               (syntax-violation #f (format "can't find file ~a in search paths"
                                            (string-append lp*/sep fp)) stx)
               (let ([full (string-append (car search) dir-sep-str lp*/sep fp)])
                 (if (file-exists? full)
                   #`(include/lexical-context ctxt #,full)
                   (loop (cdr search)))))))])))
)
