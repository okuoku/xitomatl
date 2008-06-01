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
    (xitomatl file-system base compat))
  
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
  
  (define (_directory-walk/choice who proc path top-down/bottom-up on-error)
    ;; Inspired by Python's os.walk
    (define (walk dirs)
      (for-each
        (lambda (d) 
          (_directory-walk/choice who proc d top-down/bottom-up on-error))
        (remp file-symbolic-link?
              (map (lambda (d) (path-join path d)) 
                   dirs))))
    (unless (procedure? proc)
      (assertion-violation who "not a procedure" proc))
    (unless (absolute-path? path)
      ;; Require an absolute path to always be safe.  Otherwise there are
      ;; potential problems trying to use (current-directory) because it
      ;; might be changed underneath our feet by threads, engines, or proc.
      (assertion-violation who "not an absolute path" path))
    (call/cc
      (lambda (k)
        (let-values ([(dirnames filenames)
                      (partition (lambda (d) (guard (ex [else #f])
                                               (file-directory? (path-join path d)))) 
                                 (with-exception-handler
                                   (lambda (ex) (on-error ex) (k))
                                   (lambda () (directory-list path))))])
          (case top-down/bottom-up
            [(top-down)
             (let ([dirs (proc path dirnames filenames)])
               (unless (and (list? dirs) (for-all relative-path? dirs))
                 (assertion-violation who "not a list of relative paths" dirs))
               (walk dirs))]
            [(bottom-up)
             (walk dirnames)
             (proc path dirnames filenames)]
            [else
             (assertion-violation who "not 'top-down or 'bottom-up" top-down/bottom-up)]))))
    (values))
  
  (define directory-walk/choice
    ;; When 'top-down is chosen, proc must return a list of the sub-directories
    ;; it wants the walk to descend into.  Symlinks are always ignored; if you
    ;; want to descend into one, you must call directory-walk/choice manually.
    (case-lambda
      [(proc path) 
       (directory-walk/choice proc path 'top-down)]
      [(proc path top-down/bottom-up)
       (directory-walk/choice proc path top-down/bottom-up values)]
      [(proc path top-down/bottom-up on-error)
       (_directory-walk/choice 'directory-walk/choice proc path top-down/bottom-up on-error)]))
  
  (define directory-walk
    ;; When 'top-down is chosen, all sub-directories are descended into.
    ;; Symlinks are always ignored; if you want to descend into one, you 
    ;; must call directory-walk manually.
    (case-lambda
      [(proc path)
       (directory-walk proc path 'top-down)]
      [(proc path top-down/bottom-up)
       (directory-walk proc path top-down/bottom-up values)]
      [(proc path top-down/bottom-up on-error)
       (_directory-walk/choice 'directory-walk 
         (lambda (path dirnames filenames) 
           (proc path dirnames filenames)
           dirnames) 
         path top-down/bottom-up on-error)]))
  
)
