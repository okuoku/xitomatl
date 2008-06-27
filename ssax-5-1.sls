#!r6rs
(library (xitomatl ssax (5 1))
  ;;; NOTE: The SXML created and understood by this library uses
  ;;;       & instead of @ because @ is not a valid R6RS symbol.
  (export
    ;; (xitomatl ssax parsing)
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
    Prefix-XML
    S-chars
    assert-token
    complete-start-tag
    define-labeled-arg-macro
    handle-parsed-entity
    largest-unres-name
    make-elem-parser
    make-parser/positional-args
    make-pi-parser
    make-parser
    ncname-starting-char?
    predefined-parsed-entities
    read-NCName
    read-QName
    read-attributes
    read-cdata-body
    read-char-data
    read-char-ref
    read-external-id
    read-markup-token
    read-pi-body-as-string
    resolve-name
    reverse-collect-str
    reverse-collect-str-drop-ws
    scan-Misc
    skip-S
    skip-internal-dtd
    skip-pi
    uri-string->symbol
    xml->sxml
    ;; (xitomatl ssax tree-trans)
    SRV:send-reply
    pre-post-order
    post-order
    replace-range
    ;; (xitomatl ssax html)
    sxml->html
    enattr
    entag
    string->good-html     
    make-header
    make-navbar
    make-footer
    universal-conversion-rules
    universal-protected-rules
    alist-conv-rules
    find-Header
    generic-web-rules
    ;; (xitomatl ssax sxpath)
    nodeset?
    node-typeof?
    node-eq?
    node-equal?
    node-pos
    node-filter
    take-until
    take-after
    map-union
    node-reverse
    node-trace
    select-kids
    node-self
    node-join
    node-reduce
    node-or
    node-closure
    node-parent
    sxpath)
  (import
    (xitomatl ssax parsing (5 1))
    (xitomatl ssax tree-trans (5 1))
    (xitomatl ssax html (5 1))
    (xitomatl ssax sxpath (5 1)))
)
