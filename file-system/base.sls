#!r6rs
(library (xitomatl file-system base)
  (export
    ;; From compat
    current-directory directory-list delete-directory delete-file
    make-directory make-symbolic-link change-mode
    file-exists? file-regular? file-directory? file-symbolic-link?
    ;; This library's things
    delete-directory/recursively delete-file/directory/link
    directory-walk directory-walk/choice)
  (import
    (except (rnrs) file-exists?)
    (xitomatl file-system paths)
    (xitomatl file-system base compat)
    (only (xitomatl define extras) define/?/AV case-lambda/?))
  
  (define (delete-directory/recursively fn)
    (for-each (lambda (x) 
                (delete-file/directory/link (path-join fn x)))
              (directory-list fn))
    (delete-directory fn))
    
  (define (delete-file/directory/link fn)
    (cond [(or (file-regular? fn #f) (file-symbolic-link? fn))
           (delete-file fn)]
          [(file-directory? fn #f)
           (delete-directory/recursively fn)]))
  
  (define (_directory-walk/choice AV proc abs path top-down/bottom-up on-error)
    ;; Inspired by Python's os.walk
    (define (walk dirs)
      (for-each
        (lambda (d) 
          (_directory-walk/choice AV proc abs d top-down/bottom-up on-error))
        (map (lambda (d) (path-join path d)) dirs)))
    (call/cc
      (lambda (k)
        (let-values ([(dir-names file-names symlink-names)
                      (let loop ([l (list-sort string>?
                                      (with-exception-handler
                                        (lambda (ex) (on-error ex) (k))
                                        (lambda () 
                                          (directory-list (path-join abs path)))))]
                                 [d '()] [f '()] [s '()])
                        (if (null? l)
                          (values d f s)
                          (let* ([n (car l)]
                                 [abs-n (path-join abs path n)])
                            (case (guard (ex [else 'other])
                                    (cond [(file-regular? abs-n #f) 'file]
                                          [(file-directory? abs-n #f) 'directory]
                                          [(file-symbolic-link? abs-n) 'symbolic-link]
                                          [else 'other]))
                              [(file other)
                               (loop (cdr l) d (cons n f) s)]
                              [(directory)
                               (loop (cdr l) (cons n d) f s)]
                              [(symbolic-link)
                               (loop (cdr l) d f (cons n s))]))))])
          (case top-down/bottom-up
            [(top-down)
             (let ([dirs (proc path dir-names file-names symlink-names)])
               (unless (and (list? dirs) (for-all relative-path? dirs))
                 (AV "supplied procedure did not return a list of relative paths" dirs))
               (walk dirs))]
            [(bottom-up)
             (walk dir-names)
             (proc path dir-names file-names symlink-names)]
            [else (assert #f)]))))
    (values))
  
  (define (top-down/bottom-up? x)
    (memq x '(top-down bottom-up)))
  
  (define/?/AV directory-walk/choice
    ;; When 'top-down is chosen, proc must return a list of the sub-directories
    ;; it wants the walk to descend into.
    (case-lambda/?
      [(proc path) 
       (directory-walk/choice proc path 'top-down)]
      [(proc path top-down/bottom-up)
       (directory-walk/choice proc path top-down/bottom-up values)]
      [([proc procedure?] 
        [path path?]
        [top-down/bottom-up top-down/bottom-up?]
        [on-error procedure?])
       (_directory-walk/choice 
        AV 
        proc
        (if (absolute-path? path) (root-dir-str) (current-directory)) 
        path
        top-down/bottom-up
        on-error)]))
  
  (define/?/AV directory-walk
    ;; When 'top-down is chosen, all sub-directories are descended into.
    (case-lambda/?
      [(proc path)
       (directory-walk proc path 'top-down)]
      [(proc path top-down/bottom-up)
       (directory-walk proc path top-down/bottom-up values)]
      [([proc procedure?] 
        [path path?]
        [top-down/bottom-up top-down/bottom-up?]
        [on-error procedure?])
       (_directory-walk/choice
        AV 
        (lambda (path dir-names file-names symlink-names) 
          (proc path dir-names file-names symlink-names)
          dir-names)
        (if (absolute-path? path) (root-dir-str) (current-directory))
        path
        top-down/bottom-up
        on-error)]))
  
)
