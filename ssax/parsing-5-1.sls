#!r6rs
(library (xitomatl ssax parsing (5 1))
  ;;; NOTE: The SXML created and understood by this library uses
  ;;;       & instead of @ because @ is not a valid R6RS symbol.
  (export
    make-xml-token
    xml-token-head
    xml-token-kind
    xml-token?
    make-empty-attlist
    attlist->alist
    attlist-add
    attlist-fold
    attlist-null?
    attlist-remove-top
    name-compare
    (rename 
      (ssax:Prefix-XML  Prefix-XML)
      (ssax:S-chars  S-chars)
      (ssax:assert-token  assert-token)
      (ssax:complete-start-tag  complete-start-tag)
      (ssax:define-labeled-arg-macro  define-labeled-arg-macro)
      (ssax:handle-parsed-entity  handle-parsed-entity)
      (ssax:largest-unres-name  largest-unres-name)
      (ssax:make-elem-parser  make-elem-parser)
      (ssax:make-parser/positional-args  make-parser/positional-args)
      (ssax:make-pi-parser  make-pi-parser)
      (ssax:make-parser  make-parser)
      (ssax:ncname-starting-char?  ncname-starting-char?)
      (ssax:predefined-parsed-entities  predefined-parsed-entities)
      (ssax:read-NCName  read-NCName)
      (ssax:read-QName  read-QName)
      (ssax:read-attributes  read-attributes)
      (ssax:read-cdata-body  read-cdata-body)
      (ssax:read-char-data  read-char-data)
      (ssax:read-char-ref  read-char-ref)
      (ssax:read-external-id  read-external-id)
      (ssax:read-markup-token  read-markup-token)
      (ssax:read-pi-body-as-string  read-pi-body-as-string)
      (ssax:resolve-name  resolve-name)
      (ssax:reverse-collect-str  reverse-collect-str)
      (ssax:reverse-collect-str-drop-ws  reverse-collect-str-drop-ws)
      (ssax:scan-Misc  scan-Misc)
      (ssax:skip-S  skip-S)
      (ssax:skip-internal-dtd  skip-internal-dtd)
      (ssax:skip-pi  skip-pi)
      (ssax:uri-string->symbol  uri-string->symbol)
      (ssax:xml->sxml  xml->sxml)))
  (import
    (except (rnrs) fold-right error)
    (only (xitomatl include) include/resolve)
    (except (xitomatl srfi strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash)
    (only (xitomatl control) begin0)
    (xitomatl ssax raise)
    (xitomatl ssax private-5-1 define-opt)
    (xitomatl ssax private-5-1 input-parse)
    (xitomatl ssax private-5-1 look-for-str)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 error))
  
  (define error (make-errorer "(xitomatl ssax parsing)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SSAX.scm")  
)
