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
    (xitomatl srfi and-let*)
    (xitomatl ssax private-5-1 output))
                
  (include/resolve ("xitomatl" "sxml-tools") "xpath-parser.scm")
)
