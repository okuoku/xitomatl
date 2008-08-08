#!r6rs
(library (xitomatl file-system value-file)
  (export
    value-file? value-file-path make-value-file value-file=?
    value-file-ref value-file-set! delete-value-file
    value-directory? value-directory-path make-value-directory value-directory=?
    value-directory-ref value-directory-list value-directory-set! delete-value-directory)
  (import
    (except (rnrs) file-exists?)
    (only (xitomatl define extras) define/? define/AV define/?/AV)
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
  
  (define-syntax _value-file-path
    (syntax-rules () [(_ vf) (vector-ref vf 1)]))
  
  (define-syntax _value-directory-path
    (syntax-rules () [(_ vf) (vector-ref vf 1)]))
  
  (define/? (value-file-path [vf value-file?])
    (_value-file-path vf))
  
  (define/? (value-directory-path [vd value-directory?])
    (_value-directory-path vd))
  
  (define/? (make-value-file [path path?])
    (vector 'value-file path))
  
  (define/? (make-value-directory [path path?])
    (vector 'value-directory path))
  
  (define/? (value-file=? [vf0 value-file?] [vf1 value-file?])
    (path=? (_value-file-path vf0) (_value-file-path vf1)))  
  
  (define/? (value-directory=? [vd0 value-directory?] [vd1 value-directory?])
    (path=? (_value-directory-path vd0) (_value-directory-path vd1)))  
  
  (define/? delete-value-file 
    (case-lambda/?
      [([vf value-file?])
       (delete-file (_value-file-path vf))]
      [([vd value-directory?] [vf value-file?])
       (delete-file (path-join (_value-directory-path vd) (_value-file-path vf)))]))
  
  (define/? (delete-value-directory [vd value-directory?])
    (delete-directory/recursively (_value-directory-path vd)))
  
  (define/? value-file-ref 
    (case-lambda/?
      [([vf value-file?]) 
       (call-with-input-file (_value-file-path vf) read)]
      [([vd value-directory?] [vf value-file?]) 
       (call-with-input-file 
         (path-join (_value-directory-path vd) (_value-file-path vf))
         read)]))
  
  (define/?/AV (value-directory-ref [vd value-directory?])
    (define (_value-directory-ref d)
      (map (lambda (p)
             (let ([fp (path-join d p)])
               (cond [(file-regular? fp #f)
                      (cons (make-value-file p) 
                            (call-with-input-file fp read))]
                     [(file-directory? fp #f)
                      (cons (make-value-directory p) 
                            (_value-directory-ref fp))]
                     [else (AV "not a file or directory" fp)])))
           (directory-list d)))
    (_value-directory-ref (_value-directory-path vd)))
  
  (define/?/AV (value-directory-list [vd value-directory?])
    (let ([d (_value-directory-path vd)])
      (map (lambda (p)
             (let ([fp (path-join d p)])
               (cond [(file-regular? fp #f) (make-value-file p)]
                     [(file-directory? fp #f) (make-value-directory p)]
                     [else (AV "not a file or directory" fp)])))
           (directory-list d))))
  
  (define (_value-file-set! fn v)
    (when (file-exists? fn #f)
      (delete-file/directory/link fn))
    (call-with-port (OFOP fn)
      (lambda (fop) (write v fop))))
  
  (define/? value-file-set! 
    (case-lambda/?
      [([vf value-file?] v) 
       (_value-file-set! (_value-file-path vf) v)]
      [([vd value-directory?] [vf value-file?] v)
       (_value-file-set! (path-join (_value-directory-path vd) (_value-file-path vf)) v)]))

  (define/?/AV (value-directory-set! [vd value-directory?] l)
    ;; Any duplicate path-names in l will overwrite each other, 
    ;; the last one will be the one remaining.
    (define (_value-directory-set! d l)      
      (make-directory d)
      (for-each 
        (lambda (x)
          (let ([v (car x)])
            (cond 
              [(value-file? v)
               (_value-file-set! (path-join d (_value-file-path v)) (cdr x))]
              [(value-directory? v)
               (_value-directory-set! (path-join d (_value-directory-path v))
                                      (cdr x))])))
        l))
    (letrec ([check
              (lambda (l)
                (and (list? l)
                     (for-all 
                       (lambda (x)
                         (and (pair? x)
                              (let ([v (car x)])
                                (or (and (value-file? v)
                                         #;(relative-path? (_value-file-path v)))
                                    (and (value-directory? v)
                                         #;(relative-path? (_value-directory-path v))
                                         (check (cdr x)))))))
                       l)))])
      (unless (check l) 
        (AV "not a valid association list of value-file or value-directory" l)))
    ;; The above check of l must happen first to ensure no modification of the
    ;; file-system happens if l is invalid.
    (let ([d (_value-directory-path vd)])
      (when (file-exists? d #f)
        (delete-file/directory/link d))
      (_value-directory-set! d l)))
    
  (define (OFOP fn)
    (open-file-output-port fn (file-options no-fail)
                           (buffer-mode block) (native-transcoder)))
  
)
