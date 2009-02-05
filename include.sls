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

#!r6rs
(library (xitomatl include)
  (export
    include
    include/lexical-context
    include/resolve)
  (import 
    (rnrs)
    (for (xitomatl include compat) expand)
    (for (only (xitomatl file-system paths) path-join path?) expand)
    (for (only (xitomatl exceptions) error/conditions) expand))
  
  (define-syntax include
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt filename)
         #'(include/lexical-context ctxt filename)])))
  
  (define-syntax include/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt filename)
         (and (identifier? #'ctxt)
              (or (path? (syntax->datum #'filename))
                  (syntax-violation #f "not a path" stx #'filename)))
         (let* ([fn (syntax->datum #'filename)]
                [datums 
                 (with-exception-handler
                   (lambda (ex)
                     (error/conditions 'include/lexical-context
                      "error while trying to include" (list fn)
                      (if (condition? ex) ex (make-irritants-condition (list ex)))))
                   (lambda ()
                     (call-with-input-file fn
                       (lambda (fip)
                         (let loop ([a '()])
                           (let ([x (read fip)])
                             (if (eof-object? x)
                               (reverse a)
                               (loop (cons x a)))))))))])
           (datum->syntax #'ctxt `(begin . ,datums)))])))
  
  (define-syntax include/resolve
    (lambda (stx)
      (syntax-case stx ()
        [(ctxt (lib-path* ...) file-path)
         (for-all path? (syntax->datum #'(lib-path* ... file-path)))
         (let ([p (apply path-join (syntax->datum #'(lib-path* ... file-path)))]
               [sp (search-paths)])
           (let loop ([search sp])
             (if (null? search)
               (error 'include/resolve "cannot find file in search paths" p sp)
               (let ([full (path-join (car search) p)])
                 (if (file-exists? full)
                   #`(include/lexical-context ctxt #,full)
                   (loop (cdr search)))))))])))
)
