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
(library (xitomatl fmt pretty (0 5))
  (export
    fmt-shares
    fmt-set-shares!
    fmt-copy-shares
    copy-shares
    fmt-shared-write
    join/shares
    non-app?
    syntax-abbrevs
    pp-let
    indent-rules
    indent-prefix-rules
    indent-suffix-rules
    pp-indentation
    pp-with-indent
    pp-app
    proper-non-shared-list?
    pp-flat
    pp-pair
    pp-data-list
    pp-vector
    pp-object
    pretty
    pretty/unshared)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (rnrs r5rs) quotient)
    (only (srfi :1 lists) length+)
    (only (srfi :13 strings) string-prefix? string-suffix?)
    (xitomatl include)
    (xitomatl fmt base (0 5))
    (xitomatl fmt srfi-69))

  (include/resolve ("xitomatl" "fmt") "fmt-pretty.scm")  
)
