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
(library (xitomatl lang)
  (export
    ;;; Almost all of (rnrs)
    &assertion &condition &error &i/o &i/o-decoding &i/o-encoding
    &i/o-file-already-exists &i/o-file-does-not-exist &i/o-file-is-read-only
    &i/o-file-protection &i/o-filename &i/o-invalid-position &i/o-port &i/o-read
    &i/o-write &implementation-restriction &irritants &lexical &message
    &no-infinities &no-nans &non-continuable &serious &syntax &undefined
    &violation &warning &who * + - ... / < <= = => > >= _ abs acos and append
    apply asin assert assertion-violation assertion-violation? assoc assp assq
    assv atan begin binary-port? bitwise-and bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right bitwise-bit-count
    bitwise-bit-field bitwise-bit-set? bitwise-copy-bit bitwise-copy-bit-field
    bitwise-first-bit-set bitwise-if bitwise-ior bitwise-length bitwise-not
    bitwise-reverse-bit-field bitwise-rotate-bit-field bitwise-xor boolean=?
    boolean? bound-identifier=? buffer-mode buffer-mode? bytevector->sint-list
    bytevector->string bytevector->u8-list bytevector->uint-list bytevector-copy
    bytevector-copy! bytevector-fill! bytevector-ieee-double-native-ref
    bytevector-ieee-double-native-set! bytevector-ieee-double-ref
    bytevector-ieee-double-set! bytevector-ieee-single-native-ref
    bytevector-ieee-single-native-set! bytevector-ieee-single-ref
    bytevector-ieee-single-set! bytevector-length bytevector-s16-native-ref
    bytevector-s16-native-set! bytevector-s16-ref bytevector-s16-set!
    bytevector-s32-native-ref bytevector-s32-native-set! bytevector-s32-ref
    bytevector-s32-set! bytevector-s64-native-ref bytevector-s64-native-set!
    bytevector-s64-ref bytevector-s64-set! bytevector-s8-ref bytevector-s8-set!
    bytevector-sint-ref bytevector-sint-set! bytevector-u16-native-ref
    bytevector-u16-native-set! bytevector-u16-ref bytevector-u16-set!
    bytevector-u32-native-ref bytevector-u32-native-set! bytevector-u32-ref
    bytevector-u32-set! bytevector-u64-native-ref bytevector-u64-native-set!
    bytevector-u64-ref bytevector-u64-set! bytevector-u8-ref bytevector-u8-set!
    bytevector-uint-ref bytevector-uint-set! bytevector=? bytevector? caaaar
    caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr
    cadr call-with-bytevector-output-port call-with-current-continuation
    call-with-input-file call-with-output-file call-with-port
    call-with-string-output-port call-with-values call/cc car case case-lambda
    cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr
    cdddr cddr cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<?
    char-ci=? char-ci>=? char-ci>? char-downcase char-foldcase
    char-general-category char-lower-case? char-numeric? char-title-case?
    char-titlecase char-upcase char-upper-case? char-whitespace? char<=? char<?
    char=? char>=? char>? char? close-input-port close-output-port close-port
    command-line complex? #;cond condition condition-accessor condition-irritants
    condition-message condition-predicate condition-who condition? cons cos
    current-error-port current-input-port current-output-port datum->syntax #;define
    define-condition-type define-enumeration define-record-type #;define-syntax
    #;delay delete-file denominator display div div-and-mod div0 div0-and-mod0 do
    dynamic-wind else endianness enum-set->list enum-set-complement
    enum-set-constructor enum-set-difference enum-set-indexer
    enum-set-intersection enum-set-member? enum-set-projection enum-set-subset?
    enum-set-union enum-set-universe enum-set=? #;environment eof-object eof-object?
    eol-style eq? equal-hash equal? eqv? error error-handling-mode error? #;eval
    even? exact #;exact->inexact exact-integer-sqrt exact? exists exit exp expt
    file-exists? file-options filter find finite? fixnum->flonum fixnum-width
    fixnum? fl* fl+ fl- fl/ fl<=? fl<? fl=? fl>=? fl>? flabs flacos flasin flatan
    flceiling flcos fldenominator fldiv fldiv-and-mod fldiv0 fldiv0-and-mod0
    fleven? flexp flexpt flfinite? flfloor flinfinite? flinteger? fllog flmax
    flmin flmod flmod0 flnan? flnegative? flnumerator flodd? flonum? floor
    flpositive? flround flsin flsqrt fltan fltruncate flush-output-port flzero?
    fold-left fold-right for-all for-each #;force free-identifier=? fx* fx*/carry
    fx+ fx+/carry fx- fx-/carry fx<=? fx<? fx=? fx>=? fx>? fxand
    fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right
    fxbit-count fxbit-field fxbit-set? fxcopy-bit fxcopy-bit-field fxdiv
    fxdiv-and-mod fxdiv0 fxdiv0-and-mod0 fxeven? fxfirst-bit-set fxif fxior
    fxlength fxmax fxmin fxmod fxmod0 fxnegative? fxnot fxodd? fxpositive?
    fxreverse-bit-field fxrotate-bit-field fxxor fxzero? gcd generate-temporaries
    get-bytevector-all get-bytevector-n get-bytevector-n! get-bytevector-some
    get-char get-datum get-line get-string-all get-string-n get-string-n! get-u8
    greatest-fixnum guard hashtable-clear! hashtable-contains? hashtable-copy
    hashtable-delete! hashtable-entries hashtable-equivalence-function
    hashtable-hash-function hashtable-keys hashtable-mutable? hashtable-ref
    hashtable-set! hashtable-size hashtable-update! hashtable? i/o-decoding-error?
    i/o-encoding-error-char i/o-encoding-error? i/o-error-filename i/o-error-port
    i/o-error-position i/o-error? i/o-file-already-exists-error?
    i/o-file-does-not-exist-error? i/o-file-is-read-only-error?
    i/o-file-protection-error? i/o-filename-error? i/o-invalid-position-error?
    i/o-port-error? i/o-read-error? i/o-write-error? identifier-syntax identifier?
    if imag-part implementation-restriction-violation? inexact #;inexact->exact
    inexact? infinite? input-port? integer->char integer-valued? integer?
    irritants-condition? lambda latin-1-codec lcm least-fixnum length let let*
    let*-values let-syntax let-values letrec letrec* letrec-syntax
    lexical-violation? list list->string list->vector list-ref list-sort list-tail
    list? log lookahead-char lookahead-u8 magnitude make-assertion-violation
    make-bytevector make-custom-binary-input-port
    make-custom-binary-input/output-port make-custom-binary-output-port
    make-custom-textual-input-port make-custom-textual-input/output-port
    make-custom-textual-output-port make-enumeration make-eq-hashtable
    make-eqv-hashtable make-error make-hashtable make-i/o-decoding-error
    make-i/o-encoding-error make-i/o-error make-i/o-file-already-exists-error
    make-i/o-file-does-not-exist-error make-i/o-file-is-read-only-error
    make-i/o-file-protection-error make-i/o-filename-error
    make-i/o-invalid-position-error make-i/o-port-error make-i/o-read-error
    make-i/o-write-error make-implementation-restriction-violation
    make-irritants-condition make-lexical-violation make-message-condition
    make-no-infinities-violation make-no-nans-violation
    make-non-continuable-violation make-polar make-record-constructor-descriptor
    make-record-type-descriptor make-rectangular make-serious-condition
    make-string make-syntax-violation make-transcoder make-undefined-violation
    make-variable-transformer make-vector make-violation make-warning
    make-who-condition map max member memp memq memv message-condition? min mod
    mod0 #;modulo nan? native-endianness native-eol-style native-transcoder
    negative? newline no-infinities-violation? no-nans-violation?
    non-continuable-violation? not #;null-environment null? number->string number?
    numerator odd? open-bytevector-input-port open-bytevector-output-port
    open-file-input-port open-file-input/output-port open-file-output-port
    open-input-file open-output-file open-string-input-port
    open-string-output-port or output-port-buffer-mode output-port? pair?
    partition peek-char port-eof? port-has-port-position?
    port-has-set-port-position!? port-position port-transcoder port? positive?
    procedure? put-bytevector put-char put-datum put-string put-u8 quasiquote
    quasisyntax quote #;quotient raise raise-continuable rational-valued? rational?
    rationalize read read-char real->flonum real-part real-valued? real?
    record-accessor record-constructor record-constructor-descriptor
    record-field-mutable? record-mutator record-predicate record-rtd
    record-type-descriptor record-type-descriptor? record-type-field-names
    record-type-generative? record-type-name record-type-opaque?
    record-type-parent record-type-sealed? record-type-uid record? #;remainder
    remove remp remq remv reverse round #;scheme-report-environment
    serious-condition? set! #;set-car! #;set-cdr! set-port-position! simple-conditions
    sin sint-list->bytevector sqrt standard-input-port standard-output-port string
    string->bytevector string->list string->number string->symbol string->utf16
    string->utf32 string->utf8 string-append string-ci-hash string-ci<=?
    string-ci<? string-ci=? string-ci>=? string-ci>? string-copy string-downcase
    #;string-fill! string-foldcase string-for-each string-hash string-length
    string-normalize-nfc string-normalize-nfd string-normalize-nfkc
    string-normalize-nfkd string-ref #;string-set! string-titlecase string-upcase
    string<=? string<? string=? string>=? string>? string? substring
    symbol->string symbol-hash symbol=? symbol? syntax syntax->datum syntax-case
    syntax-rules syntax-violation syntax-violation-form syntax-violation-subform
    syntax-violation? tan textual-port? transcoded-port transcoder-codec
    transcoder-eol-style transcoder-error-handling-mode truncate
    u8-list->bytevector uint-list->bytevector undefined-violation? unless unquote
    unquote-splicing unsyntax unsyntax-splicing utf-16-codec utf-8-codec
    utf16->string utf32->string utf8->string values vector vector->list
    vector-fill! vector-for-each vector-length vector-map vector-ref vector-set!
    vector-sort vector-sort! vector? violation? warning? when who-condition?
    with-exception-handler with-input-from-file with-output-to-file with-syntax
    write write-char zero?
    ;;; (rnrs) aliases
    (rename (lambda λ))
    ;;; xitomatl bindings
    ;; define extras
    define define-syntax define-values define/AV define/? define/?/AV
    ;; common
    add1 sub1 format printf fprintf gensym
    ;; ports
    read-all get-lines-all port-for-each port-map
    ;; macro-utils
    gen-temp syntax->list with-syntax*
    duplicate-id unique-ids? unique-ids?/raise formals-ok?/raise
    identifier-append identifier?/name=?
    ;; keywords
    case-lambda/kw lambda/kw define/kw
    ;; indexes
    iota enumerate
    ;; lists
    make-list last-pair map/left-right/preserving map/filter
    intersperse remove-dups remv-dups remq-dups
    ;; strings
    string-intersperse string-split string-end=?
    ;; match
    match matches? match-lambda match-lambda* match-let match-let*
    ;; predicates
    non-negative-integer? exact-non-negative-integer? positive-integer?
    exact-positive-integer? exact-integer? symbol<? name=? non-empty-string?
    char-line-ending? list-of? #;improper-list? #;datum?
    ;; SRFI-61: more general cond
    cond
    )
  (import
    (except (rnrs) define define-syntax cond)
    (xitomatl define)
    (xitomatl define extras)
    (xitomatl common)
    (xitomatl ports)
    (xitomatl macro-utils)
    (xitomatl keywords)
    (xitomatl indexes)
    (xitomatl lists)
    (xitomatl strings)
    (xitomatl match)
    (xitomatl predicates)
    (srfi :61 cond))
)
