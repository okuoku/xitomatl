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
(library (xitomatl sxml-tools ddo-txpath (2008 06 27))
  (export
    ddo:or
    ddo:foldr
    ddo:type-nodeset
    ddo:type-number
    ddo:type-string
    ddo:type-boolean
    ddo:type-any
    ddo:nset-contained?
    ddo:nset-equal?
    ddo:pos-result-forward?
    ddo:pos-result->nodeset
    ddo:location-step-pos
    ddo:location-step-non-intersect
    ddo:location-step-non-pos
    ddo:filter-expr-general
    ddo:filter-expr-non-pos
    ddo:filter-expr-special-predicate
    ddo:all-contexts-in-doc
    ddo:unite-2-contextsets
    ddo:unite-multiple-context-sets
    ddo:list-tail
    ddo:list-head
    ddo:list-ref
    ddo:check-ast-position?
    ddo:check4ast-number
    ddo:check-special-predicate
    ddo:check-ast-desc-os?
    ddo:rewrite-step*
    ddo:generate-pred-id
    ddo:get-pred-value
    ddo:get-pred-value-pos
    ddo:get-abs-lpath-value
    ddo:construct-pred-values
    ddo:construct-pred-values-pos
    ddo:vector-copy-set
    ddo:add-vector-to-var-binding
    ddo:charlst->branch
    ddo:add-var-to-tree
    ddo:var-binding->tree
    ddo:get-var-value-from-tree
    ddo:ast-axis-specifier
    ddo:ast-location-path
    ddo:ast-absolute-location-path
    ddo:ast-relative-location-path
    ddo:ast-step
    ddo:ast-step-list
    ddo:ast-predicate
    ddo:ast-predicate-list
    ddo:ast-expr
    ddo:apply-ast-procedure
    ddo:ast-or-expr
    ddo:ast-and-expr
    ddo:ast-equality-expr
    ddo:ast-relational-expr
    ddo:ast-additive-expr
    ddo:ast-multiplicative-expr
    ddo:ast-union-expr
    ddo:ast-path-expr
    ddo:ast-filter-expr
    ddo:ast-variable-reference
    ddo:ast-literal
    ddo:ast-number
    ddo:ast-function-call
    ddo:ast-function-arguments
    ddo:api-helper
    ddo:txpath
    ddo:xpath-expr
    ddo:sxpath)
  (import
    (rnrs)
    (rnrs r5rs)
    (xitomatl include)
    (only (xitomatl common) gensym)
    (srfi :2 and-let*)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools txpath)
    (xitomatl sxml-tools xpath-context--xlink)
    (xitomatl sxml-tools xpath-ast)
    (xitomatl sxml-tools ddo-axes))

  (include/resolve ("xitomatl" "sxml-tools") "ddo-txpath.scm")
)
