#!r6rs
(library (xitomatl file-system base compat)
  (export
    directory-list
    delete-directory
    file-exists?
    file-regular?
    file-directory?
    file-symbolic-link?)
  (import
    (only (ikarus) directory-list delete-directory file-exists?
                   file-regular? file-directory? file-symbolic-link?))
  ;; NOTE: Ikarus doesn't yet provide these.
)
