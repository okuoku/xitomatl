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
(library (xitomatl sxml-tools xpath-context--xlink (2008 06 27))
  ;;; This library is a combination of xpath-context and xlink because
  ;;; otherwise separately they would create an import circle.
  (export
    ;; from xpath-context
    sxml:context-u?
    sxml:context?
    sxml:context->node-u
    sxml:context->ancestors-u
    sxml:context->content-u
    sxml:context->node
    sxml:context->ancestors
    sxml:context->content
    draft:contextset->nodeset
    draft:make-context
    draft:smart-make-context
    draft:siblings->context-set
    draft:na+
    draft:na-minus
    draft:na-minus-nneg
    draft:na-max
    draft:na-min
    draft:na>
    draft:na>=
    draft:list-head
    draft:list-last
    draft:make-list
    draft:signal-semantic-error
    draft:top?
    draft:remove-eq-duplicates
    draft:reach-root
    draft:recover-contextset
    draft:find-proper-context
    draft:ancestor
    draft:ancestor-or-self
    draft:attribute
    draft:child
    draft:descendant
    draft:descendant-or-self
    draft:following
    draft:following-sibling
    draft:namespace
    draft:parent
    draft:preceding
    draft:preceding-sibling
    draft:self
    draft:core-last
    draft:core-position
    draft:core-count
    draft:core-id
    draft:core-local-name
    draft:core-namespace-uri
    draft:core-name
    draft:core-string
    draft:core-concat
    draft:core-starts-with
    draft:core-contains
    draft:core-substring-before
    draft:core-substring-after
    draft:core-substring
    draft:core-string-length
    draft:core-normalize-space
    draft:core-translate
    draft:core-boolean
    draft:core-not
    draft:core-true
    draft:core-false
    draft:core-lang
    draft:core-number
    draft:core-sum
    draft:core-floor
    draft:core-ceiling
    draft:core-round
    draft:ast-axis-specifier
    draft:ast-node-test
    draft:ast-location-path
    draft:ast-absolute-location-path
    draft:ast-relative-location-path
    draft:ast-step
    draft:ast-step-list
    draft:ast-predicate
    draft:ast-predicate-list
    draft:ast-expr
    draft:ast-or-expr
    draft:ast-and-expr
    draft:ast-equality-expr
    draft:ast-relational-expr
    draft:ast-additive-expr
    draft:ast-multiplicative-expr
    draft:ast-union-expr
    draft:ast-path-expr
    draft:ast-filter-expr
    draft:ast-variable-reference
    draft:ast-literal
    draft:ast-number
    draft:ast-function-call
    draft:ast-function-arguments
    draft:ast-xpointer
    draft:ast-child-seq
    draft:ast-number-list
    draft:ast-full-xptr
    draft:arglist->ns+na
    draft:api-helper
    draft:xpath
    draft:xpointer
    draft:xpath-expr
    draft:sxpath
    txpath-with-context
    txpath/c
    sxpath-with-context
    sxpath/c
    ;; from xlink
    xlink:ntype??
    xlink:elem-extended?
    xlink:elem-simple?
    xlink:elem-locator?
    xlink:elem-resource?
    xlink:elem-arc?
    xlink:elem-title?
    xlink:set-uri
    xlink:id-index
    xlink:arcs-declared-here
    xlink:arcs-embedded?
    xlink:arcs-outgoing
    xlink:api-error
    xlink:parser
    xlink:get-document-by-uri
    xlink:arcs-uris
    xlink:arcs-linkbase-uris
    xlink:uris
    xlink:remove-equal-duplicates
    xlink:find-doc
    xlink:referenced-uris
    xlink:referenced-linkbase-uris
    xlink:add-documents-helper
    xlink:add-linkbases-recursively
    xlink:add-documents-recursively
    xlink:get-documents-with-params
    xlink:get-documents+linkbases
    xlink:unite-duplicate-keys-in-alist
    xlink:docs-exchange-arcs
    xlink:embed-arcs-into-document
    xlink:arcs-embedded
    xlink:parameterized-load-with-respect-documents
    xlink:get-docs-with-respect-to-loaded
    xlink:load-linked-docs-with-params
    xlink:documents
    xlink:documents-embed
    sxml:document
    xlink:arc?
    xlink:docs-variable
    xlink:add-docs-to-vars
    xlink:node-embedded-arcs
    xlink:node-arcs-on-top
    xlink:node-arcs
    xlink:traverse-arcs
    xlink:axis-arc
    xlink:axis-traverse
    xlink:axis-traverse-arc)
  (import
    (rnrs)
    (rnrs r5rs)
    (xitomatl include)
    (rename (except (xitomatl srfi strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (xitomatl srfi and-let*)
    (xitomatl htmlprag)
    (xitomatl sxml-tools sxml-tools)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools xpath-parser)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools txpath)
    (xitomatl sxml-tools xpath-ast)
    (xitomatl sxml-tools xlink-parser)
    (xitomatl ssax parsing)
    (xitomatl ssax multi-parser)
    (xitomatl ssax private-5-1 util)
    (xitomatl ssax private-5-1 output))

  (include/resolve ("xitomatl" "sxml-tools") "xpath-context.scm")

  (define (open-input-resource . args)
    (error 'open-input-resource "currently not implemented"))
  
  (define (ar:resource-type . args)
    (error 'ar:resource-type "currently not implemented"))
  
  (include/resolve ("xitomatl" "sxml-tools") "xlink.scm")
)
