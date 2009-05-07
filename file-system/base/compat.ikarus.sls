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

(library (xitomatl file-system base compat)
  (export
    directory-enumerator directory-list
    current-directory delete-directory delete-file
    make-directory make-symbolic-link change-mode file-mtime file-ctime
    file-exists? file-regular? file-directory? file-symbolic-link?
    file-readable? file-writable? file-executable? file-size rename-file)
  (import
    (rnrs)
    (only (ikarus) current-directory delete-directory delete-file
                   make-directory make-symbolic-link change-mode file-mtime file-ctime
                   file-exists? file-regular? file-directory? file-symbolic-link?
                   file-readable? file-writable? file-executable? file-size)
    (only (ikarus) open-directory-stream read-directory-stream close-directory-stream)
    (prefix (only (ikarus) rename-file) ik:)
    (only (xitomatl common) format))

  (define (directory-enumerator/who who path proc seeds)
    ;; NOTE: Ikarus uses its guardians to close directory-streams which didn't
    ;; get closed before they're GC'd, which can happen if an exception is
    ;; raised in this procedure.
    (define (ignore? e) (or (string=? e ".") (string=? e "..")))
    (define (done rets)
      (close-directory-stream ds)
      (apply values rets))
    (define ds
      (with-exception-handler
        (lambda (ex)
          (raise
            (condition (make-who-condition who)
                       (if (condition? ex)
                         ex
                         (make-irritants-condition (list ex))))))
        (lambda () (open-directory-stream path))))
    (let loop ((seeds seeds))
      (let ((e (read-directory-stream ds)))
        (if e
          (if (ignore? e)
            (loop seeds)
            (let-values (((cont . next-seeds) (apply proc e seeds)))
              (if cont
                (loop next-seeds)
                (done next-seeds))))
          (done seeds)))))

  (define (directory-enumerator path proc seeds)
    (directory-enumerator/who 'directory-enumerator path proc seeds))
   
  (define (directory-list path)
    (directory-enumerator/who 'directory-list
     path
     (lambda (e a) (values #T (cons e a)))
     '(())))
 
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
