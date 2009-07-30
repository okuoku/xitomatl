#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
