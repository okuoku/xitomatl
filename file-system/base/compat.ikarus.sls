(library (xitomatl file-system base compat)
  (export
    current-directory directory-list delete-directory delete-file
    make-directory make-symbolic-link change-mode file-mtime file-ctime
    file-exists? file-regular? file-directory? file-symbolic-link?
    file-readable? file-writable? file-executable? file-size rename-file)
  (import
    (rnrs)
    (only (ikarus) current-directory delete-directory delete-file
                   make-directory make-symbolic-link change-mode file-mtime file-ctime
                   file-exists? file-regular? file-directory? file-symbolic-link?
                   file-readable? file-writable? file-executable? file-size)
    (prefix (only (ikarus) directory-list rename-file) ik:)
    (only (xitomatl common) format))

  (define (directory-list path)
    (remp (lambda (x) (member x '("." "..")))
          (ik:directory-list path)))
  
  (define rename-file 
    (case-lambda
      [(old new)
       (rename-file old new #F)]
      [(old new exists-ok)
       (when (and (not exists-ok) (file-exists? new #F))
         (raise (condition (make-who-condition 'rename-file)
                           (make-message-condition 
                            (format "already exists: ~a" new))
                           (make-i/o-filename-error old))))
       (ik:rename-file old new)]))
)
