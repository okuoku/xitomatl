#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

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
