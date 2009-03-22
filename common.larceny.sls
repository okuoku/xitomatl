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

(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time
    with-input-from-string with-output-to-string
    ;; TODO: add to as needed/appropriate
    )
  (import
    (rnrs)
    (prefix (primitives gensym) larceny:)
    (primitives pretty-print time with-input-from-string with-output-to-string)
    ;; Can't use Larceny's format because some of its directives differ.
    (prefix (srfi :48 intermediate-format-strings) IFS:))

  (define (add1 x) (+ 1 x))

  (define (sub1 x) (- x 1))

  (define (format fmt-str . fmt-args)
    (apply IFS:format #F fmt-str fmt-args))
  
  (define (printf fmt-str . fmt-args)
    (apply IFS:format #T fmt-str fmt-args))
  
  (define (fprintf port fmt-str . fmt-args)
    (unless (and (output-port? port) (textual-port? port))
      (assertion-violation 'fprintf "not a textual output port" port))
    (apply IFS:format port fmt-str fmt-args))

  (define gensym
    (case-lambda
      (()
       (larceny:gensym "gensym"))
      ((name)
       (larceny:gensym (cond ((string? name) name)
                             ((symbol? name) (symbol->string name))
                             (else (assertion-violation 'gensym
                                    "not a string or symbol" name)))))))
)
