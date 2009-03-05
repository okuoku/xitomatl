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
(library (xitomatl fmt c (0 5))
  (export
    fmt-in-macro? fmt-expression? fmt-return? fmt-default-type
    fmt-newline-before-brace? fmt-braceless-bodies?
    fmt-indent-space fmt-switch-indent-space fmt-op fmt-gen
    c-in-expr c-in-stmt c-in-test
    c-paren c-maybe-paren c-type c-literal? c-literal char->c-char
    c-struct c-union c-class c-enum c-attribute c-typedef
    c-expr c-expr/sexp c-apply c-op c-indent c-current-indent-string
    c-wrap-stmt c-open-brace c-close-brace
    c-block c-braced-block c-begin
    c-fun c-var c-prototype c-param c-param-list
    c-while c-for c-if c-switch
    c-case c-case/fallthrough c-default
    c-break c-continue c-return c-goto c-label
    c-static c-const c-extern c-volatile c-auto c-restrict c-inline
    c++ c-- c+ c- c* c/ c% c& c^ c~ c! c&& c<< c>> c== c!=
    c< c> c<= c>= c= c+= c-= c*= c/= c%= c&= c^= c<<= c>>=
    c++/post c--/post c. c->
    c-bit-or c-or c-bit-or=
    cpp-if cpp-ifdef cpp-ifndef cpp-elif cpp-endif cpp-undef
    cpp-include cpp-define cpp-wrap-header cpp-pragma cpp-line
    cpp-error cpp-warning cpp-stringify cpp-sym-cat
    c-comment c-block-comment)
  (import
    (rename (except (rnrs) error) (for-all every))
    (only (srfi :13 strings) substring/shared string-index)
    (prefix (srfi :23 error) ER:)
    (only (srfi :39 parameters) parameterize)
    (xitomatl include)
    (xitomatl fmt base (0 5)))

  (define (error . args)
    (parameterize ((ER:error-who "(library (xitomatl fmt c (0 5)))"))
      (apply ER:error args)))

  (include/resolve ("xitomatl" "fmt") "fmt-c.scm")
)
