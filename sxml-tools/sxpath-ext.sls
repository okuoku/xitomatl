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
(library (xitomatl sxml-tools sxpath-ext (2008 06 27))
  (export
    sxml:string
    sxml:boolean
    sxml:number
    sxml:string-value
    sxml:id
    sxml:nested-loop-join
    sxml:list-head
    sxml:merge-sort
    sxml:merge-sort-join
    sxml:charlst->branch
    sxml:string->tree
    sxml:add-string-to-tree
    sxml:string-in-tree?
    sxml:radix-sort-join
    sxml:equality-cmp
    sxml:equal?
    sxml:not-equal?
    sxml:relational-cmp
    sxml:ancestor
    sxml:ancestor-or-self
    sxml:descendant
    sxml:descendant-or-self
    sxml:following
    sxml:following-sibling
    sxml:namespace
    sxml:preceding
    sxml:preceding-sibling)
  (import
    (rnrs)
    (only (rnrs r5rs) exact->inexact inexact->exact)
    (xitomatl include)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools sxml-tools)
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 output))
    
  (include/resolve ("xitomatl" "sxml-tools") "sxpath-ext.scm")
)
