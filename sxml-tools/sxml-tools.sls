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
(library (xitomatl sxml-tools sxml-tools (2008 06 27))
  (export
    sxml:attr-list-node
    sxml:attr-as-list
    sxml:aux-list-node
    sxml:aux-as-list
    sxml:find-name-separator
    sxml:error
    sxml:empty-element?
    sxml:shallow-normalized?
    sxml:normalized?
    sxml:shallow-minimized?
    sxml:minimized?
    sxml:name
    sxml:element-name
    sxml:node-name
    sxml:ncname
    sxml:name->ns-id
    sxml:content
    sxml:text
    sxml:content-raw
    sxml:attr-list-u
    sxml:aux-list
    sxml:aux-list-u
    sxml:aux-node
    sxml:aux-nodes
    sxml:attr
    sxml:attr-from-list
    sxml:num-attr
    sxml:attr-u
    sxml:ns-list
    sxml:ns-id->nodes
    sxml:ns-id->uri
    sxml:ns-uri->nodes
    sxml:ns-uri->id
    sxml:ns-id
    sxml:ns-uri
    sxml:ns-prefix
    sxml:change-content!
    sxml:change-content
    sxml:change-attrlist
    sxml:change-attrlist!
    sxml:change-name!
    sxml:change-name
    sxml:add-attr
    sxml:add-attr!
    sxml:change-attr
    sxml:change-attr!
    sxml:set-attr
    sxml:set-attr!
    sxml:add-aux
    sxml:add-aux!
    sxml:squeeze!
    sxml:squeeze
    sxml:clean
    select-first-kid
    sxml:node-parent
    sxml:add-parents
    sxml:lookup
    sxml:attr->xml
    sxml:string->xml
    sxml:sxml->xml
    sxml:attr->html
    sxml:string->html
    sxml:non-terminated-html-tag?
    sxml:sxml->html)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (xitomatl include)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 util))

  (include/resolve ("xitomatl" "sxml-tools") "sxml-tools.scm")
)
