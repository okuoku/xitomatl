#!r6rs
(library (xitomatl r6rs-bindings spec)
  (export
    spec)
  (import
    (rnrs))

  (define spec
    '((null-environment
       (macros
        ... => _ and begin case cond define define-syntax
        do else if lambda let let* let-syntax letrec
        letrec-syntax or quasiquote quote set! syntax-rules
        unquote unquote-splicing)
       (variables))
      (scheme-report-environment
       (macros
        ... => _ and begin case cond define
        define-syntax delay do else if lambda let let*
        let-syntax letrec letrec-syntax or quasiquote quote
        set! syntax-rules unquote unquote-splicing)
       (variables
        * + - / < <= = > >= abs acos append apply
        asin assoc assq assv atan boolean? caaaar caaadr caaar
        caadar caaddr caadr caar cadaar cadadr cadar caddar
        cadddr caddr cadr call-with-current-continuation
        call-with-input-file call-with-output-file
        call-with-values car cdaaar cdaadr cdaar cdadar cdaddr
        cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr
        cddr cdr ceiling char->integer char-alphabetic?
        char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
        char-downcase char-lower-case? char-numeric?
        char-upcase char-upper-case? char-whitespace? char<=?
        char<? char=? char>=? char>? char? close-input-port
        close-output-port complex? cons cos current-input-port
        current-output-port denominator display dynamic-wind
        eof-object? eq? equal? eqv? eval even? exact->inexact
        exact? exp expt floor for-each force gcd imag-part
        inexact->exact inexact? integer->char integer? lcm
        length list list->string list->vector list-ref
        list-tail list? log magnitude make-polar
        make-rectangular make-string make-vector map max
        member memq memv min modulo negative? newline not
        null-environment null? number->string number?
        numerator odd? open-input-file open-output-file pair?
        peek-char positive? procedure? quotient rational?
        rationalize read read-char real-part real? remainder
        reverse round scheme-report-environment set-car!
        set-cdr! sin sqrt string string->list string->number
        string->symbol string-append string-ci<=? string-ci<?
        string-ci=? string-ci>=? string-ci>? string-copy
        string-fill! string-length string-ref string-set!
        string<=? string<? string=? string>=? string>? string?
        substring symbol->string symbol? tan truncate values
        vector vector->list vector-fill! vector-length
        vector-ref vector-set! vector? with-input-from-file
        with-output-to-file write write-char zero?))
      ((rnrs arithmetic bitwise)
       (macros)
       (variables
        bitwise-and bitwise-arithmetic-shift
        bitwise-arithmetic-shift-left
        bitwise-arithmetic-shift-right bitwise-bit-count
        bitwise-bit-field bitwise-bit-set? bitwise-copy-bit
        bitwise-copy-bit-field bitwise-first-bit-set
        bitwise-if bitwise-ior bitwise-length bitwise-not
        bitwise-reverse-bit-field bitwise-rotate-bit-field
        bitwise-xor))
      ((rnrs arithmetic fixnums)
       (macros)
       (variables
        fixnum-width fixnum? fx* fx*/carry fx+
        fx+/carry fx- fx-/carry fx<=? fx<? fx=? fx>=? fx>?
        fxand fxarithmetic-shift fxarithmetic-shift-left
        fxarithmetic-shift-right fxbit-count fxbit-field
        fxbit-set? fxcopy-bit fxcopy-bit-field fxdiv
        fxdiv-and-mod fxdiv0 fxdiv0-and-mod0 fxeven?
        fxfirst-bit-set fxif fxior fxlength fxmax fxmin fxmod
        fxmod0 fxnegative? fxnot fxodd? fxpositive?
        fxreverse-bit-field fxrotate-bit-field fxxor fxzero?
        greatest-fixnum least-fixnum))
      ((rnrs arithmetic flonums)
       (macros)
       (variables
        &no-infinities &no-nans fixnum->flonum fl*
        fl+ fl- fl/ fl<=? fl<? fl=? fl>=? fl>? flabs flacos
        flasin flatan flceiling flcos fldenominator fldiv
        fldiv-and-mod fldiv0 fldiv0-and-mod0 fleven? flexp
        flexpt flfinite? flfloor flinfinite? flinteger? fllog
        flmax flmin flmod flmod0 flnan? flnegative?
        flnumerator flodd? flonum? flpositive? flround flsin
        flsqrt fltan fltruncate flzero?
        make-no-infinities-violation make-no-nans-violation
        no-infinities-violation? no-nans-violation?
        real->flonum))
      ((rnrs base)
       (macros
        ... => _ and assert begin case cond define
        define-syntax else identifier-syntax if lambda let
        let* let*-values let-syntax let-values letrec letrec*
        letrec-syntax or quasiquote quote set! syntax-rules
        unquote unquote-splicing)
       (variables
        * + - / < <= = > >= abs acos append apply
        asin assertion-violation atan boolean=? boolean?
        caaaar caaadr caaar caadar caaddr caadr caar cadaar
        cadadr cadar caddar cadddr caddr cadr
        call-with-current-continuation call-with-values
        call/cc car cdaaar cdaadr cdaar cdadar cdaddr cdadr
        cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr
        ceiling char->integer char<=? char<? char=? char>=?
        char>? char? complex? cons cos denominator div
        div-and-mod div0 div0-and-mod0 dynamic-wind eq? equal?
        eqv? error even? exact exact-integer-sqrt exact? exp
        expt finite? floor for-each gcd imag-part inexact
        inexact? infinite? integer->char integer-valued?
        integer? lcm length list list->string list->vector
        list-ref list-tail list? log magnitude make-polar
        make-rectangular make-string make-vector map max min
        mod mod0 nan? negative? not null? number->string
        number? numerator odd? pair? positive? procedure?
        rational-valued? rational? rationalize real-part
        real-valued? real? reverse round sin sqrt string
        string->list string->number string->symbol
        string-append string-copy string-for-each
        string-length string-ref string<=? string<? string=?
        string>=? string>? string? substring symbol->string
        symbol=? symbol? tan truncate values vector
        vector->list vector-fill! vector-for-each
        vector-length vector-map vector-ref vector-set!
        vector? zero?))
      ((rnrs bytevectors)
       (macros
        endianness)
       (variables
        bytevector->sint-list bytevector->u8-list
        bytevector->uint-list bytevector-copy bytevector-copy!
        bytevector-fill! bytevector-ieee-double-native-ref
        bytevector-ieee-double-native-set!
        bytevector-ieee-double-ref bytevector-ieee-double-set!
        bytevector-ieee-single-native-ref
        bytevector-ieee-single-native-set!
        bytevector-ieee-single-ref bytevector-ieee-single-set!
        bytevector-length bytevector-s16-native-ref
        bytevector-s16-native-set! bytevector-s16-ref
        bytevector-s16-set! bytevector-s32-native-ref
        bytevector-s32-native-set! bytevector-s32-ref
        bytevector-s32-set! bytevector-s64-native-ref
        bytevector-s64-native-set! bytevector-s64-ref
        bytevector-s64-set! bytevector-s8-ref
        bytevector-s8-set! bytevector-sint-ref
        bytevector-sint-set! bytevector-u16-native-ref
        bytevector-u16-native-set! bytevector-u16-ref
        bytevector-u16-set! bytevector-u32-native-ref
        bytevector-u32-native-set! bytevector-u32-ref
        bytevector-u32-set! bytevector-u64-native-ref
        bytevector-u64-native-set! bytevector-u64-ref
        bytevector-u64-set! bytevector-u8-ref
        bytevector-u8-set! bytevector-uint-ref
        bytevector-uint-set! bytevector=? bytevector?
        make-bytevector native-endianness
        sint-list->bytevector string->utf16 string->utf32
        string->utf8 u8-list->bytevector uint-list->bytevector
        utf16->string utf32->string utf8->string))
      ((rnrs conditions)
       (macros
        define-condition-type)
       (variables
        &assertion &condition &error
        &implementation-restriction &irritants &lexical
        &message &non-continuable &serious &syntax &undefined
        &violation &warning &who assertion-violation?
        condition condition-accessor condition-irritants
        condition-message condition-predicate condition-who
        condition? error?
        implementation-restriction-violation?
        irritants-condition? lexical-violation?
        make-assertion-violation make-error
        make-implementation-restriction-violation
        make-irritants-condition make-lexical-violation
        make-message-condition make-non-continuable-violation
        make-serious-condition make-syntax-violation
        make-undefined-violation make-violation make-warning
        make-who-condition message-condition?
        non-continuable-violation? serious-condition?
        simple-conditions syntax-violation-form
        syntax-violation-subform syntax-violation?
        undefined-violation? violation? warning?
        who-condition?))
      ((rnrs control)
       (macros
        case-lambda do unless when)
       (variables))
      ((rnrs enums)
       (macros
        define-enumeration)
       (variables
        enum-set->list enum-set-complement
        enum-set-constructor enum-set-difference
        enum-set-indexer enum-set-intersection
        enum-set-member? enum-set-projection enum-set-subset?
        enum-set-union enum-set-universe enum-set=?
        make-enumeration))
      ((rnrs eval)
       (macros)
       (variables
        environment eval))
      ((rnrs exceptions)
       (macros
        guard)
       (variables
        raise raise-continuable
        with-exception-handler))
      ((rnrs files)
       (macros)
       (variables
        delete-file file-exists?))
      ((rnrs hashtables)
       (macros)
       (variables
        equal-hash hashtable-clear!
        hashtable-contains? hashtable-copy hashtable-delete!
        hashtable-entries hashtable-equivalence-function
        hashtable-hash-function hashtable-keys
        hashtable-mutable? hashtable-ref hashtable-set!
        hashtable-size hashtable-update! hashtable?
        make-eq-hashtable make-eqv-hashtable make-hashtable
        string-ci-hash string-hash symbol-hash))
      ((rnrs io ports)
       (macros
        buffer-mode eol-style error-handling-mode)
       (variables
        &i/o &i/o-decoding &i/o-encoding
        &i/o-file-already-exists &i/o-file-does-not-exist
        &i/o-file-is-read-only &i/o-file-protection
        &i/o-filename &i/o-invalid-position &i/o-port
        &i/o-read &i/o-write binary-port? buffer-mode?
        bytevector->string call-with-bytevector-output-port
        call-with-port call-with-string-output-port close-port
        current-error-port current-input-port
        current-output-port eof-object eof-object?
        file-options flush-output-port get-bytevector-all
        get-bytevector-n get-bytevector-n! get-bytevector-some
        get-char get-datum get-line get-string-all
        get-string-n get-string-n! get-u8 i/o-decoding-error?
        i/o-encoding-error-char i/o-encoding-error?
        i/o-error-filename i/o-error-port i/o-error-position
        i/o-error? i/o-file-already-exists-error?
        i/o-file-does-not-exist-error?
        i/o-file-is-read-only-error?
        i/o-file-protection-error? i/o-filename-error?
        i/o-invalid-position-error? i/o-port-error?
        i/o-read-error? i/o-write-error? input-port?
        latin-1-codec lookahead-char lookahead-u8
        make-custom-binary-input-port
        make-custom-binary-input/output-port
        make-custom-binary-output-port
        make-custom-textual-input-port
        make-custom-textual-input/output-port
        make-custom-textual-output-port
        make-i/o-decoding-error make-i/o-encoding-error
        make-i/o-error make-i/o-file-already-exists-error
        make-i/o-file-does-not-exist-error
        make-i/o-file-is-read-only-error
        make-i/o-file-protection-error make-i/o-filename-error
        make-i/o-invalid-position-error make-i/o-port-error
        make-i/o-read-error make-i/o-write-error
        make-transcoder native-eol-style native-transcoder
        open-bytevector-input-port open-bytevector-output-port
        open-file-input-port open-file-input/output-port
        open-file-output-port open-string-input-port
        open-string-output-port output-port-buffer-mode
        output-port? port-eof? port-has-port-position?
        port-has-set-port-position!? port-position
        port-transcoder port? put-bytevector put-char
        put-datum put-string put-u8 set-port-position!
        standard-input-port standard-output-port
        string->bytevector textual-port? transcoded-port
        transcoder-codec transcoder-eol-style
        transcoder-error-handling-mode utf-16-codec
        utf-8-codec))
      ((rnrs io simple)
       (macros)
       (variables
        &i/o &i/o-file-already-exists
        &i/o-file-does-not-exist &i/o-file-is-read-only
        &i/o-file-protection &i/o-filename
        &i/o-invalid-position &i/o-port &i/o-read &i/o-write
        call-with-input-file call-with-output-file
        close-input-port close-output-port current-error-port
        current-input-port current-output-port display
        eof-object eof-object? i/o-error-filename
        i/o-error-port i/o-error-position i/o-error?
        i/o-file-already-exists-error?
        i/o-file-does-not-exist-error?
        i/o-file-is-read-only-error?
        i/o-file-protection-error? i/o-filename-error?
        i/o-invalid-position-error? i/o-port-error?
        i/o-read-error? i/o-write-error? input-port?
        make-i/o-error make-i/o-file-already-exists-error
        make-i/o-file-does-not-exist-error
        make-i/o-file-is-read-only-error
        make-i/o-file-protection-error make-i/o-filename-error
        make-i/o-invalid-position-error make-i/o-port-error
        make-i/o-read-error make-i/o-write-error newline
        open-input-file open-output-file output-port?
        peek-char read read-char with-input-from-file
        with-output-to-file write write-char))
      ((rnrs lists)
       (macros)
       (variables
        assoc assp assq assv exists filter find
        fold-left fold-right for-all member memp memq memv
        partition remove remp remq remv))
      ((rnrs mutable-pairs)
       (macros)
       (variables
        set-car! set-cdr!))
      ((rnrs mutable-strings)
       (macros)
       (variables
        string-fill! string-set!))
      ((rnrs programs)
       (macros)
       (variables
        command-line exit))
      ((rnrs r5rs)
       (macros
        delay)
       (variables
        exact->inexact force inexact->exact modulo
        null-environment quotient remainder
        scheme-report-environment))
      ((rnrs records inspection)
       (macros)
       (variables
        record-field-mutable? record-rtd
        record-type-field-names record-type-generative?
        record-type-name record-type-opaque?
        record-type-parent record-type-sealed? record-type-uid
        record?))
      ((rnrs records procedural)
       (macros)
       (variables
        make-record-constructor-descriptor
        make-record-type-descriptor record-accessor
        record-constructor record-mutator record-predicate
        record-type-descriptor?))
      ((rnrs records syntactic)
       (macros
        define-record-type record-constructor-descriptor
        record-type-descriptor)
       (variables))
      ((rnrs sorting)
       (macros)
       (variables
        list-sort vector-sort vector-sort!))
      ((rnrs syntax-case)
       (macros
        ... _ quasisyntax syntax syntax-case unsyntax
        unsyntax-splicing with-syntax)
       (variables
        bound-identifier=? datum->syntax
        free-identifier=? generate-temporaries identifier?
        make-variable-transformer syntax->datum
        syntax-violation))
      ((rnrs unicode)
       (macros)
       (variables
        char-alphabetic? char-ci<=? char-ci<?
        char-ci=? char-ci>=? char-ci>? char-downcase
        char-foldcase char-general-category char-lower-case?
        char-numeric? char-title-case? char-titlecase
        char-upcase char-upper-case? char-whitespace?
        string-ci<=? string-ci<? string-ci=? string-ci>=?
        string-ci>? string-downcase string-foldcase
        string-normalize-nfc string-normalize-nfd
        string-normalize-nfkc string-normalize-nfkd
        string-titlecase string-upcase))
      ((rnrs)
       (macros
        ... => _ and assert begin buffer-mode case
        case-lambda cond define define-condition-type
        define-enumeration define-record-type define-syntax do
        else endianness eol-style error-handling-mode guard
        identifier-syntax if lambda let let* let*-values
        let-syntax let-values letrec letrec* letrec-syntax or
        quasiquote quasisyntax quote
        record-constructor-descriptor record-type-descriptor
        set! syntax syntax-case syntax-rules unless unquote
        unquote-splicing unsyntax unsyntax-splicing when
        with-syntax)
       (variables
        &assertion &condition &error &i/o
        &i/o-decoding &i/o-encoding &i/o-file-already-exists
        &i/o-file-does-not-exist &i/o-file-is-read-only
        &i/o-file-protection &i/o-filename
        &i/o-invalid-position &i/o-port &i/o-read &i/o-write
        &implementation-restriction &irritants &lexical
        &message &no-infinities &no-nans &non-continuable
        &serious &syntax &undefined &violation &warning &who *
        + - / < <= = > >= abs acos append apply asin
        assertion-violation assertion-violation? assoc assp
        assq assv atan binary-port? bitwise-and
        bitwise-arithmetic-shift bitwise-arithmetic-shift-left
        bitwise-arithmetic-shift-right bitwise-bit-count
        bitwise-bit-field bitwise-bit-set? bitwise-copy-bit
        bitwise-copy-bit-field bitwise-first-bit-set
        bitwise-if bitwise-ior bitwise-length bitwise-not
        bitwise-reverse-bit-field bitwise-rotate-bit-field
        bitwise-xor boolean=? boolean? bound-identifier=?
        buffer-mode? bytevector->sint-list bytevector->string
        bytevector->u8-list bytevector->uint-list
        bytevector-copy bytevector-copy! bytevector-fill!
        bytevector-ieee-double-native-ref
        bytevector-ieee-double-native-set!
        bytevector-ieee-double-ref bytevector-ieee-double-set!
        bytevector-ieee-single-native-ref
        bytevector-ieee-single-native-set!
        bytevector-ieee-single-ref bytevector-ieee-single-set!
        bytevector-length bytevector-s16-native-ref
        bytevector-s16-native-set! bytevector-s16-ref
        bytevector-s16-set! bytevector-s32-native-ref
        bytevector-s32-native-set! bytevector-s32-ref
        bytevector-s32-set! bytevector-s64-native-ref
        bytevector-s64-native-set! bytevector-s64-ref
        bytevector-s64-set! bytevector-s8-ref
        bytevector-s8-set! bytevector-sint-ref
        bytevector-sint-set! bytevector-u16-native-ref
        bytevector-u16-native-set! bytevector-u16-ref
        bytevector-u16-set! bytevector-u32-native-ref
        bytevector-u32-native-set! bytevector-u32-ref
        bytevector-u32-set! bytevector-u64-native-ref
        bytevector-u64-native-set! bytevector-u64-ref
        bytevector-u64-set! bytevector-u8-ref
        bytevector-u8-set! bytevector-uint-ref
        bytevector-uint-set! bytevector=? bytevector? caaaar
        caaadr caaar caadar caaddr caadr caar cadaar cadadr
        cadar caddar cadddr caddr cadr
        call-with-bytevector-output-port
        call-with-current-continuation call-with-input-file
        call-with-output-file call-with-port
        call-with-string-output-port call-with-values call/cc
        car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar
        cddaar cddadr cddar cdddar cddddr cdddr cddr cdr
        ceiling char->integer char-alphabetic? char-ci<=?
        char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase
        char-foldcase char-general-category char-lower-case?
        char-numeric? char-title-case? char-titlecase
        char-upcase char-upper-case? char-whitespace? char<=?
        char<? char=? char>=? char>? char? close-input-port
        close-output-port close-port command-line complex?
        condition condition-accessor condition-irritants
        condition-message condition-predicate condition-who
        condition? cons cos current-error-port
        current-input-port current-output-port datum->syntax
        delete-file denominator display div div-and-mod div0
        div0-and-mod0 dynamic-wind enum-set->list
        enum-set-complement enum-set-constructor
        enum-set-difference enum-set-indexer
        enum-set-intersection enum-set-member?
        enum-set-projection enum-set-subset? enum-set-union
        enum-set-universe enum-set=? eof-object eof-object?
        eq? equal-hash equal? eqv? error error? even? exact
        exact-integer-sqrt exact? exists exit exp expt
        file-exists? file-options filter find finite?
        fixnum->flonum fixnum-width fixnum? fl* fl+ fl- fl/
        fl<=? fl<? fl=? fl>=? fl>? flabs flacos flasin flatan
        flceiling flcos fldenominator fldiv fldiv-and-mod
        fldiv0 fldiv0-and-mod0 fleven? flexp flexpt flfinite?
        flfloor flinfinite? flinteger? fllog flmax flmin flmod
        flmod0 flnan? flnegative? flnumerator flodd? flonum?
        floor flpositive? flround flsin flsqrt fltan
        fltruncate flush-output-port flzero? fold-left
        fold-right for-all for-each free-identifier=? fx*
        fx*/carry fx+ fx+/carry fx- fx-/carry fx<=? fx<? fx=?
        fx>=? fx>? fxand fxarithmetic-shift
        fxarithmetic-shift-left fxarithmetic-shift-right
        fxbit-count fxbit-field fxbit-set? fxcopy-bit
        fxcopy-bit-field fxdiv fxdiv-and-mod fxdiv0
        fxdiv0-and-mod0 fxeven? fxfirst-bit-set fxif fxior
        fxlength fxmax fxmin fxmod fxmod0 fxnegative? fxnot
        fxodd? fxpositive? fxreverse-bit-field
        fxrotate-bit-field fxxor fxzero? gcd
        generate-temporaries get-bytevector-all
        get-bytevector-n get-bytevector-n! get-bytevector-some
        get-char get-datum get-line get-string-all
        get-string-n get-string-n! get-u8 greatest-fixnum
        hashtable-clear! hashtable-contains? hashtable-copy
        hashtable-delete! hashtable-entries
        hashtable-equivalence-function hashtable-hash-function
        hashtable-keys hashtable-mutable? hashtable-ref
        hashtable-set! hashtable-size hashtable-update!
        hashtable? i/o-decoding-error? i/o-encoding-error-char
        i/o-encoding-error? i/o-error-filename i/o-error-port
        i/o-error-position i/o-error?
        i/o-file-already-exists-error?
        i/o-file-does-not-exist-error?
        i/o-file-is-read-only-error?
        i/o-file-protection-error? i/o-filename-error?
        i/o-invalid-position-error? i/o-port-error?
        i/o-read-error? i/o-write-error? identifier? imag-part
        implementation-restriction-violation? inexact inexact?
        infinite? input-port? integer->char integer-valued?
        integer? irritants-condition? latin-1-codec lcm
        least-fixnum length lexical-violation? list
        list->string list->vector list-ref list-sort list-tail
        list? log lookahead-char lookahead-u8 magnitude
        make-assertion-violation make-bytevector
        make-custom-binary-input-port
        make-custom-binary-input/output-port
        make-custom-binary-output-port
        make-custom-textual-input-port
        make-custom-textual-input/output-port
        make-custom-textual-output-port make-enumeration
        make-eq-hashtable make-eqv-hashtable make-error
        make-hashtable make-i/o-decoding-error
        make-i/o-encoding-error make-i/o-error
        make-i/o-file-already-exists-error
        make-i/o-file-does-not-exist-error
        make-i/o-file-is-read-only-error
        make-i/o-file-protection-error make-i/o-filename-error
        make-i/o-invalid-position-error make-i/o-port-error
        make-i/o-read-error make-i/o-write-error
        make-implementation-restriction-violation
        make-irritants-condition make-lexical-violation
        make-message-condition make-no-infinities-violation
        make-no-nans-violation make-non-continuable-violation
        make-polar make-record-constructor-descriptor
        make-record-type-descriptor make-rectangular
        make-serious-condition make-string
        make-syntax-violation make-transcoder
        make-undefined-violation make-variable-transformer
        make-vector make-violation make-warning
        make-who-condition map max member memp memq memv
        message-condition? min mod mod0 nan? native-endianness
        native-eol-style native-transcoder negative? newline
        no-infinities-violation? no-nans-violation?
        non-continuable-violation? not null? number->string
        number? numerator odd? open-bytevector-input-port
        open-bytevector-output-port open-file-input-port
        open-file-input/output-port open-file-output-port
        open-input-file open-output-file
        open-string-input-port open-string-output-port
        output-port-buffer-mode output-port? pair? partition
        peek-char port-eof? port-has-port-position?
        port-has-set-port-position!? port-position
        port-transcoder port? positive? procedure?
        put-bytevector put-char put-datum put-string put-u8
        raise raise-continuable rational-valued? rational?
        rationalize read read-char real->flonum real-part
        real-valued? real? record-accessor record-constructor
        record-field-mutable? record-mutator record-predicate
        record-rtd record-type-descriptor?
        record-type-field-names record-type-generative?
        record-type-name record-type-opaque?
        record-type-parent record-type-sealed? record-type-uid
        record? remove remp remq remv reverse round
        serious-condition? set-port-position!
        simple-conditions sin sint-list->bytevector sqrt
        standard-input-port standard-output-port string
        string->bytevector string->list string->number
        string->symbol string->utf16 string->utf32
        string->utf8 string-append string-ci-hash string-ci<=?
        string-ci<? string-ci=? string-ci>=? string-ci>?
        string-copy string-downcase string-foldcase
        string-for-each string-hash string-length
        string-normalize-nfc string-normalize-nfd
        string-normalize-nfkc string-normalize-nfkd string-ref
        string-titlecase string-upcase string<=? string<?
        string=? string>=? string>? string? substring
        symbol->string symbol-hash symbol=? symbol?
        syntax->datum syntax-violation syntax-violation-form
        syntax-violation-subform syntax-violation? tan
        textual-port? transcoded-port transcoder-codec
        transcoder-eol-style transcoder-error-handling-mode
        truncate u8-list->bytevector uint-list->bytevector
        undefined-violation? utf-16-codec utf-8-codec
        utf16->string utf32->string utf8->string values vector
        vector->list vector-fill! vector-for-each
        vector-length vector-map vector-ref vector-set!
        vector-sort vector-sort! vector? violation? warning?
        who-condition? with-exception-handler
        with-input-from-file with-output-to-file write
        write-char zero?))))

)
