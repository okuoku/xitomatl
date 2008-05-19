#!r6rs
(library (xitomatl file-system base compat)
  (export
    directory-list
    (rename (mz:delete-directory delete-directory)
            (mz:make-directory make-directory))
    file-exists?
    file-regular?
    file-directory?
    file-symbolic-link?)
  (import
    (except (rnrs) file-exists?)
    (prefix (only (scheme base) 
                  link-exists? directory-exists? path->string make-directory
                  directory-list delete-directory file-exists?) 
            mz:)
    (prefix (only (scheme mpair) list->mlist) mz:))
  
  (define (directory-list path)
    (map mz:path->string (mz:list->mlist (mz:directory-list path))))
  
  (define file-exists?
    (case-lambda 
      [(path) (file-exists? path #t)]
      [(path follow)
       (if follow
         (or (mz:file-exists? path)
             (mz:directory-exists? path))
         (or (mz:link-exists? path)
             (mz:file-exists? path)
             (mz:directory-exists? path)))]))
  
  (define file-regular? 
    (case-lambda
      [(path) (file-regular? path #t)]
      [(path follow)
       (if follow
         (mz:file-exists? path)
         (and (not (mz:link-exists? path))
              (not (mz:directory-exists? path))
              (mz:file-exists? path)))]))
  
  (define file-directory?
    (case-lambda 
      [(path) (file-directory? path #t)]
      [(path follow)
       (if follow
         (mz:directory-exists? path)
         (and (not (mz:link-exists? path))
              (not (mz:file-exists? path))
              (mz:directory-exists? path)))]))
  
  (define (file-symbolic-link? path)
    (mz:link-exists? path))
)
