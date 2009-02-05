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
(library (xitomatl strings)
  (export
    string-intersperse
    string-split whitespace
    string-end=?
    ;; from (xitomatl strings compat)
    string-copy!)
  (import
    (rnrs)
    (only (xitomatl lists) intersperse)
    (xitomatl strings compat))
  
  (define (string-intersperse sl ssep)
    (apply string-append (intersperse sl ssep)))
  
  (define whitespace 
    (apply string
           '(#\space #\linefeed #\return #\tab #\vtab #\page #\x85 #\xA0 
             #\x1680 #\x180E #\x2000 #\x2001 #\x2002 #\x2003 #\x2004 #\x2005
             #\x2006 #\x2007 #\x2008 #\x2009 #\x200A #\x2028 #\x2029 #\x202F
             #\x205F #\x3000)))
  
  (define string-split
    (case-lambda
      [(str) 
       (string-split str whitespace #f)]
      [(str delim-strs)
       (string-split str delim-strs #f)]
      [(str delim-strs keep-empty)
       (unless (and (string? str) (string? delim-strs))
         (assertion-violation 'string-split "not a string" 
                              (if (string? delim-strs) str delim-strs)))
       (let ([strlen (string-length str)]
             [dellen (string-length delim-strs)])
         (let loop ([i (- strlen 1)]
                    [to strlen]
                    [accum '()])
           (if (< i 0)
             (if (or (< 0 to) keep-empty)
               (cons (substring str 0 to) accum)
               accum)
             (let ([c (string-ref str i)])
               (let check ([j 0])
                 (cond [(= j dellen) (loop (- i 1) to accum)]
                       [(char=? c (string-ref delim-strs j))
                        (loop (- i 1) i (let ([i+1 (+ i 1)])
                                          (if (or (< i+1 to) keep-empty)
                                            (cons (substring str i+1 to) accum)
                                            accum)))]
                       [else (check (+ j 1))]))))))]))
  
  (define (string-end=? str end)
    (let ([sl (string-length str)]
          [el (string-length end)])
      (and (>= sl el)
           (string=? (substring str (- sl el) sl) end))))
)
