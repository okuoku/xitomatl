;;; Copyright (c) 2008 Derick Eddington
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; Except as contained in this notice, the name(s) of the above copyright
;;; holders shall not be used in advertising or otherwise to promote the sale,
;;; use or other dealings in this Software without prior written authorization.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!r6rs
(library (xitomatl IU-match macro-helpers)
  (export
    find-ids/prevent-auto-recur
    check-ids/prevent-dups-across
    check-ids/dups-okay)
  (import
    (rnrs))
  
  (define (problem form-stx kw-name-stx msg subform)
    (syntax-violation #f msg
      (syntax-case form-stx () [(_ ctxt . rest) #`(#,kw-name-stx . rest)])
      subform))  
  
  (define (find-ids/prevent-auto-recur form-stx kw-name-stx pat)
    ;; Destructures a match pattern syntax to extract pattern variable identifiers.
    ;; Detects attempted uses of match's auto-recursion ability because it doesn't
    ;; make any sense for matching using a single pattern only.
    ;; Returns a list of the extracted syntax-object identifiers.
    (define-syntax self-recur
      (syntax-rules ()
        [(_ x) (find-ids/prevent-auto-recur form-stx kw-name-stx x)]))
    (define (not-an-id x) 
      (problem form-stx kw-name-stx "not an identifier" x))
    (syntax-case pat (unquote)
      [(unquote id/recur)
       (if (identifier? #'id/recur)
         (list #'id/recur)
         (not-an-id #'id/recur))]
      [((unquote id/recur) . rest)
       (if (identifier? #'id/recur)
         (cons #'id/recur (self-recur #'rest))
         (not-an-id #'id/recur))]
      [(any . rest)
       (append (self-recur #'any) (self-recur #'rest))]
      [#((unquote id/recur) rest ...)
       (if (identifier? #'id/recur)
         (cons #'id/recur (self-recur #'(rest ...)))
         (not-an-id #'id/recur))]
      [#(any rest ...)
       (append (self-recur #'any) (self-recur #'(rest ...)))]
      [atom '()]))
  
  (define (check-ids/prevent-dups-across form-stx kw-name-stx pat*)
    ;; Extracts all pattern variable identifiers in the supplied pattern syntaxes and
    ;; prevents attempted uses of match's auto-recursion, using find-ids/prevent-auto-recur,
    ;; and prevents duplicate identifiers accross the patterns, while allowing 
    ;; duplicate identifiers in a single pattern.
    ;; Returns #t if all checks pass.
    (define pat*-ids 
      (map (lambda (pat) (find-ids/prevent-auto-recur form-stx kw-name-stx pat)) pat*))
    (if (null? pat*-ids)
      #t
      (let loop ([first (car pat*-ids)] [others (cdr pat*-ids)])
        (if (null? others)
          #t
          (begin
            (for-each 
              (lambda (fid)
                (for-each 
                  (lambda (other)
                    (for-each 
                      (lambda (oid) 
                        (when (bound-identifier=? fid oid)
                          (problem form-stx kw-name-stx "duplicate binding across patterns" fid))) 
                      other))
                  others))
              first)
            (loop (car others) (cdr others)))))))
  
  (define (check-ids/dups-okay form-stx kw-name-stx pat*)
    ;; Uses find-ids/prevent-auto-recur to prevent attempted use of match's 
    ;; auto-recursion, and allows duplicate pattern variable identifiers
    ;; across patterns.
    ;; Returns #t if the checks pass.
    (for-each 
      (lambda (pat) (find-ids/prevent-auto-recur form-stx kw-name-stx pat))
      pat*)
    #t)   
)
