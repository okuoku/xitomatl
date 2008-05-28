#!r6rs
(library (xitomatl file-system value-file)
  (export
    value-file? value-file-path make-value-file value-file=?
    value-file-ref value-file-set! delete-value-file
    value-directory? value-directory-path make-value-directory value-directory=?
    value-directory-ref #;value-directory-set! delete-value-directory)
  (import
    (except (rnrs) file-exists?)
    (only (xitomatl define extras) define/? define/AV)
    (xitomatl file-system paths)
    (xitomatl file-system base))
  
  (define (value-file? a)
    (and (vector? a)
         (= 2 (vector-length a))
         (eq? 'value-file (vector-ref a 0))
         (path? (vector-ref a 1))))
  
  (define (value-directory? a)
    (and (vector? a)
         (= 2 (vector-length a))
         (eq? 'value-directory (vector-ref a 0))
         (path? (vector-ref a 1))))
  
  (define/? (value-file-path [vf value-file?])
    (vector-ref vf 1))
  
  (define/? (value-directory-path [vd value-directory?])
    (vector-ref vd 1))
  
  (define/? (make-value-file [path path?])
    (vector 'value-file path))
  
  (define/? (make-value-directory [path path?])
    (vector 'value-directory path))
  
  (define (value-file=? vf0 vf1)
    (string=? (value-file-path vf0) (value-file-path vf1)))  
  
  (define (value-directory=? vd0 vd1)
    (string=? (value-directory-path vd0) (value-directory-path vd1)))  
  
  (define delete-value-file 
    (case-lambda
      [(vf)
       (delete-file (value-file-path vf))]
      [(vd vf)
       (delete-file (path-join (value-directory-path vd) (value-file-path vf)))]))
  
  (define (delete-value-directory vd)
    (delete-directory/recursively (value-directory-path vd)))
  
  (define value-file-ref 
    (let ([f (lambda (fn) (call-with-input-file fn read))])
      (case-lambda
        [(vf) (f (value-file-path vf))]
        [(vd vf) (f (path-join (value-directory-path vd) (value-file-path vf)))])))
  
  (define/AV (value-directory-ref vd)
    (map (lambda (fn)
           (cond [(file-regular? fn #f)
                  (make-value-file fn)]
                 [(file-directory? fn #f)
                  (make-value-directory fn)]
                 [else 
                  (AV "not a file or directory" fn)]))
         (directory-list (value-directory-path vd))))
  
  (define value-file-set! 
    (let ([f (lambda (fn v)
               (when (file-exists? fn #f)
                 (delete-file/directory/link fn))
               (call-with-port (OFOP fn)
                 (lambda (fop) (write v fop))))])
      (case-lambda
        [(vf v) (f (value-file-path vf) v)]
        [(vd vf v) (f (path-join (value-directory-path vd) (value-file-path vf)) v)])))

  #;(define/AV (value-directory-set! vd alrp)
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
