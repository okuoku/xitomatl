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
(library (xitomatl sxml-tools sxpathlib (2008 06 27))
  (export
    nodeset?
    as-nodeset
    sxml:element?
    ntype-names??
    ntype??
    ntype-namespace-id??
    sxml:complement
    node-eq?
    node-equal?
    node-pos
    sxml:filter
    take-until
    take-after
    map-union
    node-reverse
    node-trace
    select-kids
    node-self
    node-join
    node-reduce
    node-or
    node-closure
    sxml:node?
    sxml:attr-list
    sxml:attribute
    sxml:child
    sxml:parent
    node-parent
    sxml:child-nodes
    sxml:child-elements)
  (import
    (rnrs)
    (xitomatl include)
    (rename (except (srfi :13 strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 output)
    (rename (only (xitomatl common) pretty-print)
            (pretty-print pp)))

  (include/resolve ("xitomatl" "sxml-tools") "sxpathlib.scm")
)
