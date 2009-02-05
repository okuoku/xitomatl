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
(library (xitomatl sxml-tools txpath (2008 06 27))
  (export
    sxml:xpointer-runtime-error
    sxml:xpath-nodeset-filter
    sxml:arithmetic-eval
    sxml:core-last
    sxml:core-position
    sxml:core-count
    sxml:core-id
    sxml:core-local-name
    sxml:core-namespace-uri
    sxml:core-name
    sxml:core-string
    sxml:core-concat
    sxml:core-starts-with
    sxml:core-contains
    sxml:core-substring-before
    sxml:core-substring-after
    sxml:core-substring
    sxml:core-string-length
    sxml:core-normalize-space
    sxml:core-translate
    sxml:core-boolean
    sxml:core-not
    sxml:core-true
    sxml:core-false
    sxml:core-lang
    sxml:core-number
    sxml:core-sum
    sxml:core-floor
    sxml:core-ceiling
    sxml:core-round
    sxml:classic-params
    sxml:api-helper0
    sxml:classic-res
    sxml:api-helper
    sxml:xpath
    sxml:xpointer
    sxml:xpath-expr
    sxml:xpath+root+vars
    sxml:xpointer+root+vars
    sxml:xpath+root
    txpath
    sxml:api-index-helper
    sxml:xpath+index
    sxml:xpointer+index)
  (import
    (rnrs)
    (rnrs r5rs)
    (xitomatl include)
    (rename (except (srfi :13 strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (xitomatl sxml-tools xpath-parser)
    (xitomatl sxml-tools sxml-tools)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 util))

  (include/resolve ("xitomatl" "sxml-tools") "txpath.scm")
)
