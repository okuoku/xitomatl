#!r6rs
(library (xitomatl file-system base compat)
  (export
    current-directory directory-list delete-directory delete-file
    make-directory make-symbolic-link change-mode
    file-exists? file-regular? file-directory? file-symbolic-link?)
  (import
    (rnrs)
    (only (ikarus) current-directory delete-directory delete-file
                   make-directory make-symbolic-link change-mode
                   file-exists? file-regular? file-directory? file-symbolic-link?)
    (prefix (only (ikarus) directory-list) ik:))

  (define (directory-list path)
    (remp (lambda (x) (member x '("." "..")))
          (ik:directory-list path)))
  
)
