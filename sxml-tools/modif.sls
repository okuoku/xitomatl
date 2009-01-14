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
(library (xitomatl sxml-tools modif (2008 06 27))
  (export
    sxml:modification-error
    sxml:separate-list
    sxml:assert-proper-attribute
    sxml:unite-annot-attributes-lists
    sxml:tree-trans
    sxml:transform-document
    sxml:lambdas-upd-specifiers->targets
    sxml:update-specifiers->lambdas
    modif:insert-following
    modif:insert-preceding
    modif:insert-into
    modif:rename
    modif:delete
    modif:delete-undeep
    sxml:modify
    sxml:clone
    sxml:clone-nset-except
    sxml:replace-next-with-lst!
    sxml:mutate-doc!
    sxml:nodes-to-mutate
    sxml:modify!)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (xitomatl include)
    (srfi :2 and-let*)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools xpath-context--xlink)
    (xitomatl sxml-tools ddo-txpath)
    (xitomatl sxml-tools xpath-ast)
    (xitomatl ssax private-5-1 output))

  (include/resolve ("xitomatl" "sxml-tools") "modif.scm")
)
