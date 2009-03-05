;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

#!r6rs
(library (xitomatl fmt base (0 5))
  (export
    call-with-output-string
    write-to-string
    display-to-string
    nl-str
    make-space
    make-nl-space
    take*
    drop*
    *default-fmt-state*
    fmt-state?
    new-fmt-state
    copy-fmt-state
    fmt-row
    fmt-col
    fmt-radix
    fmt-properties
    fmt-pad-char
    fmt-precision
    fmt-width
    fmt-writer
    fmt-port
    fmt-decimal-sep
    fmt-string-width
    fmt-ellipses
    fmt-set-row!
    fmt-set-col!
    fmt-set-radix!
    fmt-set-properties!
    fmt-set-pad-char!
    fmt-set-precision!
    fmt-set-width!
    fmt-set-writer!
    fmt-set-port!
    fmt-set-decimal-sep!
    fmt-set-string-width!
    fmt-set-ellipses!
    fmt-ref
    fmt-set-property!
    fmt-set!
    fmt-add-properties!
    fmt-let
    fmt-bind
    fix
    radix
    pad-char
    comma-char
    decimal-char
    with-width
    ellipses
    fmt-start
    fmt
    fmt-update
    fmt-write
    apply-cat
    cat
    fmt-null
    fmt-if
    fmt-try-fit
    fits-in-width
    fits-in-columns
    fmt-capture
    fmt-to-string
    nl
    fl
    tab-to
    space-to
    join
    join/prefix
    join/suffix
    join/last
    join/dot
    join/range
    pad/both
    pad
    pad/right
    pad/left
    trim/buffered
    trim
    trim/length
    trim/left
    trim/both
    fit
    fit/left
    fit/both
    make-string-fmt-transformer
    upcase
    downcase
    titlecase
    *min-e*
    *bot-f*
    integer-log
    integer-length*
    invlog2of
    fast-expt
    mirror-of
    num->string
    num
    num/comma
    num/si
    num/fit
    eq?-table-ref
    eq?-table-set!
    make-shared-ref-table
    gen-shared-ref
    maybe-gen-shared-ref
    call-with-shared-ref
    call-with-shared-ref/cdr
    slashified
    maybe-slashified
    fmt-write-string
    dsp
    write-with-shares
    wrt
    wrt/unshared)
  (import
    (except (rnrs) error)
    (rnrs mutable-pairs)
    (only (rnrs r5rs) exact->inexact inexact->exact modulo quotient remainder)
    (srfi :6 basic-string-ports)
    (only (srfi :13 strings) substring/shared string-index string-index-right
                             string-count string-concatenate-reverse)
    (prefix (srfi :23 error) ER:)
    (only (srfi :39 parameters) parameterize)
    (xitomatl include)
    (xitomatl fmt let-optionals*)
    (xitomatl fmt srfi-69))
  
  (define (error . args)
    (parameterize ((ER:error-who "(library (xitomatl fmt base (0 5)))"))
      (apply ER:error args)))

  (define (mantissa+exponent num . opt)
    ;; Break a positive real number down to a normalized mantissa and
    ;; exponent. Default base=2, mant-size=52, exp-size=11 for IEEE doubles.
    (if (zero? num)
      (list 0 0)
      (let-optionals* opt ((base 2) (mant-size 52) (exp-size 11))
        (let* ((bot (expt base mant-size))
               (top (* base bot)))
          (let lp ((n num) (e 0))
            (cond
              ((>= n top) (lp (quotient n base) (+ e 1)))
              ((< n bot) (lp (* n base) (- e 1)))
              (else (list n e))))))))
  
  (include/resolve ("xitomatl" "fmt") "fmt.scm")
)
