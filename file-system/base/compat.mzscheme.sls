#!r6rs
(library (xitomatl file-system base compat)
  (export
    current-directory
    directory-list
    delete-file
    delete-directory
    (rename (mz:make-file-or-directory-link make-symbolic-link))
    make-directory
    change-mode
    file-exists?
    file-regular?
    file-directory?
    file-symbolic-link?)
  (import
    (except (rnrs) file-exists?)
    (prefix (only (scheme base) 
                  link-exists? directory-exists? path->string make-directory
                  directory-list delete-directory file-exists? 
                  make-file-or-directory-link current-directory
                  with-handlers exn:fail:filesystem? exn-message) 
            mz:)
    (prefix (only (scheme mpair) list->mlist) mz:))
  
  (define current-directory
    (case-lambda
      [() (mz:path->string (mz:current-directory))]
      [(x) (mz:current-directory x)]))
  
  (define (directory-list path)
    (define who 'directory-list)
    (map mz:path->string 
         (mz:list->mlist
          (mz:with-handlers ([mz:exn:fail:filesystem? 
                              (lambda (ex)
                                (raise (condition 
                                        (make-i/o-filename-error path)
                                        (make-who-condition who)
                                        (make-message-condition (mz:exn-message ex)))))])
            (mz:directory-list path)))))
  
  (define delete-directory
    (case-lambda
      [(path) 
       (delete-directory path #f)]
      [(path want-error)
       (define who 'delete-directory)
       (let ([r (mz:with-handlers ([mz:exn:fail:filesystem? mz:exn-message])
                  (mz:delete-directory path)
                  #t)])
         (if want-error
           (unless (eq? r #t) 
             (raise (condition (make-i/o-filename-error path)
                               (make-who-condition who)
                               (make-message-condition r))))
           (eq? r #t)))]))
  
  (define make-directory
    (case-lambda 
      [(path) 
       (mz:make-directory path)]
      [(path mode)       
       (unless (fixnum? mode)
         (assertion-violation 'make-directory "not a fixnum" mode))
       (make-directory path)
       (change-mode path mode)]))
  
  (define (change-mode path mode)
    (define who 'change-mode)
    (unless (string? path)
      (assertion-violation who "not a string" path))
    (unless (fixnum? mode)
      (assertion-violation who "not a fixnum" mode))
    (assertion-violation who "not implemented")
    #|(Use MzScheme's FFI to use C chmod, I guess)|#)
  
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
