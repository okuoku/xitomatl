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
(library (xitomatl sxml-tools ddo-axes (2008 06 27))
  (export
    ddo:list-last
    ddo:attr-child
    ddo:attrs-and-values
    ddo:discard-attributes
    ddo:ancestor
    ddo:ancestor-or-self
    ddo:attribute
    ddo:child
    ddo:descendant
    ddo:descendant-or-self
    ddo:following
    ddo:following-sibling
    ddo:namespace
    ddo:parent
    ddo:preceding
    ddo:preceding-sibling
    ddo:self
    ddo:following-single-level
    ddo:following-sibling-single-level
    ddo:parent-single-level
    ddo:preceding-single-level
    ddo:preceding-sibling-single-level
    ddo:ancestor-pos
    ddo:ancestor-or-self-pos
    ddo:child-pos
    ddo:descendant-pos
    ddo:descendant-or-self-pos
    ddo:following-sibling-pos
    ddo:parent-pos
    ddo:preceding-sibling-pos
    ddo:following-single-level-pos
    ddo:following-sibling-single-level-pos
    ddo:parent-single-level-pos
    ddo:preceding-single-level-pos
    ddo:preceding-sibling-single-level-pos)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools xpath-context--xlink))

  (include/resolve ("xitomatl" "sxml-tools") "ddo-axes.scm")
)
