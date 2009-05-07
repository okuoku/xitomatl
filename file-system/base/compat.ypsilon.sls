;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; Derived from code made by Ken Dickey and Y. Fujita which they gave to me.

(library (xitomatl file-system base compat)
  (export
    (rename (yp:current-directory current-directory))
    directory-enumerator directory-list delete-directory delete-file
    make-directory make-symbolic-link change-mode file-mtime file-ctime
    file-exists? file-regular? file-directory? file-symbolic-link?
    file-readable? file-writable? file-executable? file-size rename-file)
  (import
    (except (rnrs) delete-file file-exists?)
    (prefix (only (core)
                  current-directory directory-list delete-file create-directory
                  create-symbolic-link change-file-mode file-stat-mtime
                  file-stat-ctime file-exists? file-regular? file-directory?
                  file-symbolic-link? file-readable? file-writable?
                  file-executable? rename-file file-size-in-bytes)
            yp:)
    (only (xitomatl exceptions) catch)
    (only (xitomatl define) define/who define/?)
    (only (xitomatl file-system paths) path?)
    (only (xitomatl common) format))

  (define (raise-IO-F-error who msg path)
    (raise (condition (make-who-condition who)
                      (make-message-condition msg)
                      (make-i/o-filename-error path))))

  (define (handle-io-ex who fn)
    (lambda (ex)
      (if (i/o-error? ex)
        (raise-IO-F-error who (if (message-condition? ex)
                                (condition-message ex)
                                "underlying Ypsilon procedure failed")
                          fn)
        (raise ex))))

  (define-syntax with-reraise-io
    (syntax-rules ()
      ((_ who fn expr ...)
       (with-exception-handler
         (handle-io-ex who fn)
         (lambda () expr ...)))))

  (define (directory-enumerator . _)
    (assertion-violation 'directory-enumerator "not implemented"))
  
  (define/who (directory-list path)
    (remp (lambda (x) (member x '("." "..")))
          (with-reraise-io who path
            (yp:directory-list path))))

  (define/who make-directory
    (case-lambda
      ((path)
       (with-reraise-io who path
         (yp:create-directory path)))
      ((path mode)
       (make-directory path)
       (change-mode path mode))))

  (define/? rename-file
    (case-lambda/?
      ((old new) (rename-file old new #F))
      ((old (new path?) exists-ok)
       (define who 'rename-file)
       (when (and (not exists-ok) (file-exists? new #F))
         (raise-IO-F-error who (format "already exists: ~a" new) old))
       (with-reraise-io who old
         (yp:rename-file old new)))))

  (define/who (make-symbolic-link target path)
    (with-reraise-io who path
      (yp:create-symbolic-link target path)))

  (define/? delete-directory
    (case-lambda/?
      ((path) (delete-directory path #F))
      (((path path?) want-error)
       (define who 'delete-directory)
       (if (file-directory? path)
         (if want-error
           (with-reraise-io who path
             (yp:delete-file path))
           (catch ex (((i/o-error? ex) #F))
             (yp:delete-file path)
             #T))
         (and want-error
              (raise-IO-F-error who "not a directory" path))))))

  (define/? (delete-file (path path?))
    (define who 'delete-file)
    (when (file-directory? path #F)
      (raise-IO-F-error who "is a directory" path))
    (with-reraise-io who path
      (yp:delete-file path)))

  (define/who (change-mode path mode)
    (with-reraise-io who path
      (yp:change-file-mode path mode)))

  (define/who (file-mtime path)
    (with-reraise-io who path
      (yp:file-stat-mtime path)))

  (define/who (file-ctime path)
    (with-reraise-io who path
      (yp:file-stat-ctime path)))

  (define/who (file-size path)
    (with-reraise-io who path
      (yp:file-size-in-bytes path)))

  (define/who file-exists?
    (case-lambda
      ((path) (file-exists? path #T))
      ((path follow)
       (if follow
         (with-reraise-io who path
           (yp:file-exists? path))
         (or (file-exists? path #T)
             (file-symbolic-link? path))))))

  (define/who file-regular?
    (case-lambda
      ((path) (file-regular? path #T))
      ((path follow)
       (if follow
         (with-reraise-io who path
           (yp:file-regular? path))
         (and (file-regular? path #T)
              (not (file-symbolic-link? path)))))))

  (define/who file-directory?
    (case-lambda
      ((path) (file-directory? path #T))
      ((path follow)
       (if follow
         (with-reraise-io who path
           (yp:file-directory? path))
         (and (file-directory? path #T)
              (not (file-symbolic-link? path)))))))

  (define/who (file-symbolic-link? path)
    (with-reraise-io who path
      (yp:file-symbolic-link? path)))

  (define-syntax define-perm?
    (syntax-rules ()
      ((_ name perm?)
       (define/? (name (path path?))
         (unless (file-exists? path)
           (raise-IO-F-error 'name "file does not exist" path))
         (with-reraise-io 'name path
           (perm? path))))))

  (define-perm? file-readable? yp:file-readable?)

  (define-perm? file-writable? yp:file-writable?)

  (define-perm? file-executable? yp:file-executable?)
)
