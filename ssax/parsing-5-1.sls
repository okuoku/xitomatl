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
(library (xitomatl ssax parsing (5 1))
  ;;; NOTE: The SXML created and understood by this library uses
  ;;;       ^ instead of @ because @ is not a valid R6RS symbol.
  (export
    make-xml-token
    xml-token?
    xml-token-kind
    xml-token-head
    ssax:S-chars
    ssax:skip-S
    ssax:ncname-starting-char?
    ssax:read-NCName
    ssax:read-QName
    ssax:Prefix-XML
    name-compare
    ssax:largest-unres-name
    ssax:read-markup-token
    ssax:skip-pi
    ssax:read-pi-body-as-string
    ssax:skip-internal-dtd
    ssax:read-cdata-body
    ssax:read-char-ref
    ssax:predefined-parsed-entities
    ssax:handle-parsed-entity
    make-empty-attlist
    attlist-add
    attlist-null?
    attlist-remove-top
    attlist->alist
    attlist-fold
    ssax:read-attributes
    ssax:resolve-name
    ssax:uri-string->symbol
    ssax:complete-start-tag
    ssax:read-external-id
    ssax:scan-Misc
    ssax:read-char-data
    ssax:assert-token
    ssax:make-pi-parser
    ssax:make-elem-parser
    ssax:make-parser
    ssax:make-parser/positional-args
    ssax:define-labeled-arg-macro
    ssax:reverse-collect-str
    ssax:reverse-collect-str-drop-ws
    ssax:xml->sxml
    SSAX:XML->SXML)
  (import
    (except (rnrs) fold-right error)
    (xitomatl include)
    (except (xitomatl srfi strings) string-copy string->list string-titlecase
            string-upcase string-downcase string-hash string-for-each)
    (only (xitomatl control) begin0)
    (xitomatl ssax raise)
    (xitomatl ssax private-5-1 define-opt)
    (xitomatl ssax private-5-1 input-parse)
    (xitomatl ssax private-5-1 look-for-str)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 error)
    (xitomatl ssax private-5-1 util))
  
  (define error (make-errorer "(xitomatl ssax parsing)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SSAX.scm")  
)
