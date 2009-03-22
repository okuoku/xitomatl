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
    current-directory directory-list delete-directory delete-file
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
   directory-list delete-directory delete-file
   make-directory make-symbolic-link change-mode file-mtime file-ctime
   file-exists? file-regular? file-directory? file-symbolic-link?
   file-readable? file-writable? file-executable? file-size rename-file)
)
