#!r6rs
(library (xitomatl ssax multi-parser)
  (export
    ssax:multi-parser)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (xitomatl include)
    (rename (except (xitomatl srfi strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (xitomatl ssax parsing)
    (xitomatl ssax private-5-1 util)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax raise)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools xlink-parser))
  
  (define (open-input-resource . args)
    (assertion-violation 'open-input-resource
      "currently not implemented"))
  
  (include/resolve ("xitomatl" "ssax" "private-plt") "ssax-prim.ss")
  (include/resolve ("xitomatl" "ssax" "private-plt") "id.ss")
  (include/resolve ("xitomatl" "ssax" "private-plt") "multi-parser.ss")
)
