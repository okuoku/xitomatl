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
(library (xitomatl irregex (0 7 2))
  (export
    irregex string->irregex sre->irregex irregex? irregex-match-data?
    irregex-new-matches irregex-reset-matches!
    irregex-match-start-source irregex-match-start-index
    irregex-match-end-source irregex-match-end-index
    irregex-match-num-submatches irregex-match-substring irregex-match-index
    irregex-search irregex-search/matches irregex-match
    irregex-replace irregex-replace/all irregex-fold
    irregex-search/chunked irregex-match/chunked
    make-irregex-chunker irregex-match-subchunk
    irregex-dfa irregex-dfa/search irregex-dfa/extract
    irregex-nfa irregex-flags irregex-num-submatches irregex-lengths irregex-names
    irregex-quote irregex-opt sre->string string->sre maybe-string->sre
    ;; Needed by (xitomatl irregex extras)
    irregex-match-start-source-set! irregex-match-end-source-set!
    chunker-get-start chunker-get-end chunker-get-subchunk
    string-cat-reverse)
  (import
    (except (rnrs) error remove)
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (only (xitomatl include) include/resolve)
    (only (xitomatl strings) string-intersperse)
    (only (xitomatl common) with-output-to-string))

  (define (error . args)
    (apply assertion-violation "(library (xitomatl irregex (0 7 2)))" args))

  (define-syntax any
    (syntax-rules ()
      [(_ pred ls)
       (exists pred ls)]))

  (define-syntax every
    (syntax-rules ()
      [(_ pred ls)
       (for-all pred ls)]))

  (define-syntax remove
    (syntax-rules ()
      [(_ pred ls)
       (remp pred ls)]))

  (define-syntax ->string
    (syntax-rules ()
      [(_ expr)
       expr]))

  (include/resolve ("xitomatl" "irregex") "irregex-r6rs.scm")
  (include/resolve ("xitomatl" "irregex") "irregex-utils.scm")
)
