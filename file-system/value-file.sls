#!r6rs
(library (xitomatl file-system value-file)
  (export
    value-file? value-file-path make-value-file value-file=?
    value-file-ref value-file-set! delete-value-file
    value-directory? value-directory-path make-value-directory value-directory=?
    value-directory-ref value-directory-set! delete-value-directory)
  (import
    (except (rnrs) file-exists?)
    (only (xitomatl define extras) define/? define/AV)
    (xitomatl srfi parameters)
    (xitomatl common-unstandard)
    (xitomatl file-system paths)
    (xitomatl file-system base))
  
  (define (value-file? a)
    (and (vector? a)
         (= 2 (vector-length a))
         (eq? 'value-file (vector-ref a 0))
         (absolute-path? (vector-ref a 1))))
  
  (define (value-directory? a)
    (and (vector? a)
         (= 2 (vector-length a))
         (eq? 'value-directory (vector-ref a 0))
         (absolute-path? (vector-ref a 1))))
  
  (define/? (value-file-path [vf value-file?])
    (vector-ref vf 1))
  
  (define/? (value-directory-path [vd value-directory?])
    (vector-ref vd 1))
  
  (define/? (make-value-file [path absolute-path?])
    (vector 'value-file path))
  
  (define/? (make-value-directory [path absolute-path?])
    (vector 'value-directory path))
  
  (define (value-file=? vf0 vf1)
    (string=? (value-file-path vf0) (value-file-path vf1)))  
  
  (define (value-directory=? vd0 vd1)
    (string=? (value-directory-path vd0) (value-directory-path vd1)))  
  
  (define (delete-value-file vf)
    (delete-file (value-file-path vf)))
  
  (define (delete-value-directory vd)
    (delete-directory/recursively (value-directory-path vd)))
  
  (define (value-file-ref vf)
    (call-with-input-file (value-file-path vf) read))
  
  (define/AV (value-directory-ref vd)
    (map (lambda (filename.entity)
           (let ([e (cdr filename.entity)] [fn (car filename.entity)])
             (cond [(file? e)
                    (cons fn (make-value-file (entity-absolute-path e)))]
                   [(directory? e)
                    (cons fn (make-value-directory (entity-absolute-path e)))]
                   [else 
                    (AV "internal bug: not a file or directory" e)])))
         (directory->alist (value-directory-path vd))))
  
  (define (value-file-set! vf v)
    (let ([fn (value-file-path vf)])
      (when (file-exists? fn #f)
        (delete-file/directory/link fn))
      (call-with-port (OFOP fn)
        (lambda (fop) (write v fop)))))

  (define/AV (value-directory-set! vd alrp)
    (define (alist-of-relative-paths? x)
      (and (list? x)
           (for-all (lambda (y) 
                      (and (pair? y) (relative-path? (car y))))
                    x)))
    (unless (alist-of-relative-paths? alrp)
      (AV "not an association-list of relative-path keys" alrp))
    (let ([fn (value-directory-path vd)])
      (when (file-exists? fn #f)
        (delete-file/directory/link fn))
      (make-directory fn)
      (for-each 
        (lambda (ap)
          (call-with-port (OFOP (path-join fn (car ap)))
            (lambda (fop) (write (cdr ap) fop))))
        alrp))
    )
    
  (define (OFOP fn)
    (open-file-output-port fn (file-options no-fail)
                           (buffer-mode block) (native-transcoder)))
  
)
