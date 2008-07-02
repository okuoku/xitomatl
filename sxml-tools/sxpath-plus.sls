#!r6rs
(library (xitomatl sxml-tools sxpath-plus (2008 06 27))
  (export
    analyze-red1
    analyze-reduce
    analyze-1
    analyze-step
    analyze-path
    sxpath+)
  (import
    (rename (rnrs) (syntax->datum syntax-object->datum))
    (xitomatl include))

  (include/resolve ("xitomatl" "sxml-tools") "sxpath-plus.scm")
)
