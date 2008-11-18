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
(library (xitomatl sxml-tools serializer (2008 06 27))
  (export
    srl:map-append
    srl:apply-string-append
    srl:assoc-cdr-string=
    srl:member-ci
    srl:mem-pred
    srl:char-nl
    srl:newline
    srl:select-kids
    srl:separate-list
    srl:clean-fragments
    srl:display-fragments-2nesting
    srl:split-name
    srl:atomic->string
    srl:empty-elem?
    srl:conventional-ns-prefixes
    srl:namespace-assoc-for-elem
    srl:ns-assoc-for-top
    srl:extract-original-prefix-binding
    srl:update-space-specifier
    srl:normalize-sequence
    srl:xml-char-escaped
    srl:string->cdata-section
    srl:escape-alist-char-data
    srl:escape-alist-att-value
    srl:escape-alist-html-att
    srl:string->escaped
    srl:string->char-data
    srl:string->att-value
    srl:string->html-att
    srl:shtml-entity->char-data
    srl:qname->string
    srl:attribute->str-lst
    srl:namespace-decl->str-lst
    srl:comment->str-lst
    srl:processing-instruction->str-lst
    srl:name->qname-components
    srl:construct-start-end-tags
    srl:node->nested-str-lst-recursive
    srl:display-node-out-recursive
    srl:make-xml-decl
    srl:top->nested-str-lst
    srl:display-top-out
    srl:sxml->string
    srl:display-sxml
    srl:parameterizable
    srl:sxml->xml
    srl:sxml->xml-noindent
    srl:sxml->html
    srl:sxml->html-noindent)
  (import
    (rnrs)
    (xitomatl include))

  (include/resolve ("xitomatl" "sxml-tools") "serializer.scm")
)
