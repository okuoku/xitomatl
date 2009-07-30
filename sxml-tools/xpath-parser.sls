#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl sxml-tools xpath-parser (2008 06 27))
  (export
    txp:param-value
    txp:error?
    sxml:xpointer-parse-error
    sxml:xpointer-parse-warning
    txp:semantic-errs-detected?
    txp:signal-semantic-error
    sxml:whitespace
    sxml:delimiter
    sxml:non-first?
    sxml:skip-ws
    sxml:assert-end-of-path
    sxml:parse-check
    sxml:parse-check-sequence
    sxml:parse-assert
    sxml:parse-ncname
    sxml:parse-name
    sxml:parse-qname
    sxml:parse-natural
    sxml:parse-literal
    sxml:parse-number
    txp:resolve-ns-prefix
    txp:parameterize-parser)
  (import
    (rnrs)
    (rnrs r5rs)
    (xitomatl include)
    (srfi :2 and-let*)
    (xitomatl ssax private-5-1 output))
                
  (include/resolve ("xitomatl" "sxml-tools") "xpath-parser.scm")
)
