#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl file-system base compat)
  (export
    directory-enumerator directory-list
    current-directory delete-directory delete-file
    make-directory make-symbolic-link change-mode file-mtime file-ctime
    file-exists? file-regular? file-directory? file-symbolic-link?
    file-readable? file-writable? file-executable? file-size rename-file)
  (import
    (rnrs base)
    (primitives current-directory))

  ;; TODO.  Larceny does not provide much file-system interface.  I think most
  ;;        of the procedures will need to be implemented using Larceny's FFI.

  (define-syntax not-implemented
    (syntax-rules ()
      ((_ name ...)
       (begin
         (define (name . args)
           (assertion-violation 'name "not implemented"))
         ...))))

  (not-implemented
   directory-enumerator directory-list delete-directory delete-file
   make-directory make-symbolic-link change-mode file-mtime file-ctime
   file-exists? file-regular? file-directory? file-symbolic-link?
   file-readable? file-writable? file-executable? file-size rename-file)
)
