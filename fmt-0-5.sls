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
(library (xitomatl fmt (0 5))
  (export
    new-fmt-state
    fmt fmt-start fmt-if fmt-capture fmt-let fmt-bind fmt-null
    fmt-ref fmt-set! fmt-add-properties! fmt-set-property!
    fmt-col fmt-set-col! fmt-row fmt-set-row!
    fmt-radix fmt-set-radix! fmt-precision fmt-set-precision!
    fmt-properties fmt-set-properties! fmt-width fmt-set-width!
    fmt-writer fmt-set-writer! fmt-port fmt-set-port!
    fmt-decimal-sep fmt-set-decimal-sep!
    copy-fmt-state
    fmt-file fmt-try-fit cat apply-cat nl fl nl-str
    join join/last join/dot join/prefix join/suffix join/range
    pad pad/right pad/left pad/both trim trim/left trim/both trim/length
    fit fit/left fit/both tab-to space-to wrt wrt/unshared dsp
    pretty pretty/unshared slashified maybe-slashified
    num num/si num/fit num/comma radix fix ellipses
    upcase downcase titlecase pad-char comma-char decimal-char
    with-width wrap-lines fold-lines justify
    make-string-fmt-transformer
    make-space make-nl-space display-to-string write-to-string
    fmt-columns columnar line-numbers)
  (import
    (xitomatl fmt base (0 5))
    (xitomatl fmt pretty (0 5))
    (xitomatl fmt column (0 5)))
)
