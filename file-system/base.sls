#!r6rs
(library (xitomatl file-system base)
  (export
    ;; From compat
    directory-list make-directory delete-directory
    file-exists? file-regular? file-directory? file-symbolic-link?
    ;; This library's things
    entity? file? directory? link? entity-absolute-path
    path->entity delete-directory/recursively delete-file/directory/link
    directory->alist)
  (import
    (except (rnrs) file-exists?)
    (only (xitomatl define extras) define/AV)
    (xitomatl file-system paths)
    (xitomatl file-system base compat))
  
  (define-record-type entity (fields absolute-path))
  (define-record-type file (parent entity))
  (define-record-type (directory _make-directory directory?) (parent entity))
  (define-record-type link (parent entity))
  
  (define/AV (path->entity e)
    (cond
      [(file-regular? e #f) (make-file e)]
      [(file-directory? e #f) (_make-directory e)] 
      [(file-symbolic-link? e) (make-link e)]
      [else (AV "no such file, directory, or link" e)]))
  
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
  
  (define (directory->alist dname)
    (map
      (lambda (e) 
        (cons e (path->entity (path-join dname e))))
      (directory-list dname)))
)
