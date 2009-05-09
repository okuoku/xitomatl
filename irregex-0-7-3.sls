;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl irregex (0 7 3))
  (export
    irregex string->irregex sre->irregex irregex? irregex-match-data?
    irregex-new-matches irregex-reset-matches!
    irregex-match-start-source irregex-match-start-index
    irregex-match-end-source irregex-match-end-index
    irregex-match-num-submatches irregex-match-substring irregex-match-index
    irregex-search irregex-search/matches irregex-match
    irregex-replace irregex-replace/all irregex-fold irregex-fold/chunked
    irregex-search/chunked irregex-match/chunked
    make-irregex-chunker irregex-match-subchunk
    irregex-dfa irregex-dfa/search irregex-dfa/extract
    irregex-nfa irregex-flags irregex-num-submatches irregex-lengths irregex-names
    irregex-quote irregex-opt sre->string string->sre maybe-string->sre
    ;; Needed by (xitomatl irregex extras)
    irregex-match-start-source-set! irregex-match-end-source-set!
    chunker-get-next chunker-get-str chunker-get-start
    chunker-get-end chunker-get-substring chunker-get-subchunk
    string-cat-reverse)
  (import
    (rename (except (rnrs) error remove)
            (exists any) (for-all every) (remp remove))
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (only (xitomatl include) include/resolve)
    (only (xitomatl strings) string-intersperse)
    (only (xitomatl common) with-output-to-string))

  (define (error . args)
    (apply assertion-violation "(library (xitomatl irregex (0 7 3)))" args))

  (define-syntax ->string
    (syntax-rules ()
      [(_ expr)
       expr]))

  (include/resolve ("xitomatl" "irregex") "irregex-r6rs.scm")
  (include/resolve ("xitomatl" "irregex") "irregex-utils.scm")
)
