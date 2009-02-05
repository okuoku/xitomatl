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
(library (xitomatl strings compat)
  (export
    string-copy!)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (xitomatl define) define/AV))

  (define/AV (string-copy! src src-start dst dst-start k)
    ;; Taken from Ikarus's implementation of string-copy!
    (cond
      [(or (not (fixnum? src-start)) (fx<? src-start 0))
       (AV "not a valid starting index" src-start)]
      [(or (not (fixnum? dst-start)) (fx<? dst-start 0))
       (AV "not a valid starting index" dst-start)]
      [(or (not (fixnum? k)) (fx<? k 0))
       (AV "not a valid length" k)]
      [(not (string? src)) 
       (AV "not a string" src)]
      [(not (string? dst)) 
       (AV "not a string" dst)]
      [(fx>? (fx+ src-start k) (string-length src))
       (AV "out of range" src-start k)]
      [(fx>? (fx+ dst-start k) (string-length dst))
       (AV "out of range" dst-start k)]
      [(eq? src dst)
       (cond
         [(fx<? dst-start src-start)
          (let f ([src src] [si src-start] [di dst-start] [sj (fx+ src-start k)])
            (unless (fx=? si sj)
              (string-set! src di (string-ref src si))
              (f src (fx+ 1 si) (fx+ 1 di) sj)))]
         [(fx<? src-start dst-start)
          (let f ([src src] [si (fx+ src-start k)] [di (fx+ dst-start k)] [sj src-start])
            (unless (fx=? si sj)
              (let ([si (fx- si 1)] [di (fx- di 1)])
                (string-set! src di (string-ref src si))
                (f src si di sj))))]
         [else (values)])]
      [else
       (let f ([src src] [si src-start] [dst dst] [di dst-start] [sj (fx+ src-start k)])
         (unless (fx=? si sj)
           (string-set! dst di (string-ref src si))
           (f src (fx+ 1 si) dst (fx+ 1 di) sj)))]))

)
