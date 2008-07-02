#!r6rs
(library (xitomatl sxml-tools sxpath (2008 06 27))
  (export
    sxpath
    if-sxpath
    if-car-sxpath
    car-sxpath
    sxml:id-alist)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl srfi and-let*)
    (xitomatl sxml-tools sxml-tools)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools txpath)
    (xitomatl ssax private-5-1 output))

  (include/resolve ("xitomatl" "sxml-tools") "sxpath.scm")
)
