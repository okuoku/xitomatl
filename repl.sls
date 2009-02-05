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
(library (xitomatl repl)
  (export
    repl)
  (import 
    (except (rnrs) current-input-port current-output-port current-error-port)
    (rnrs eval)
    (only (xitomatl define) define/?)
    (only (xitomatl exceptions) print-exception catch reraise)
    (only (srfi :39 parameters) parameterize)
    (only (xitomatl common) pretty-print)
    (only (xitomatl ports) textual-input-port? textual-output-port?)
    (only (xitomatl environments) environment?)
    (xitomatl repl compat))
  
  
  (define/? repl 
    (case-lambda/?
      [(in out err env)
       (repl in out err env (lambda () "> "))]
      [([in textual-input-port?] 
        [out textual-output-port?]
        [err textual-output-port?]
        [env environment?]
        [prompt procedure?])
       (define (print-ex ex)
         (flush-output-port out)
         (print-exception ex err)
         (flush-output-port err))
       (let loop ()
         (display (prompt) out)
         (flush-output-port out)
         (let ([x (catch ex ([(lexical-violation? ex)
                              (print-ex ex)
                              (display "\nQuiting REPL.\n" err)
                              (flush-output-port err)
                              (eof-object)])
                    (read in))])
           (cond
             [(eof-object? x) 
              (newline out)
              (flush-output-port out)
              (values)]
             [else
              (call/cc
               (lambda (k)
                 (call-with-values
                  (lambda () 
                    (with-exception-handler
                      (lambda (ex)
                        (if (non-continuable-violation? ex)
                          (begin (print-ex ex) 
                                 (k))
                          (reraise ex)))
                      (lambda ()
                        (with-exception-handler
                          (lambda (ex)
                            (print-ex ex)
                            (when (serious-condition? ex)
                              (k))
                            (values))
                          (lambda ()
                            (parameterize ([current-input-port in]
                                           [current-output-port out]
                                           [current-error-port err])
                              (eval x env)))))))
                  (lambda vals
                    (for-each (lambda (v) (pretty-print v out))
                              vals)
                    (flush-output-port out)))))
              (loop)])))]))
)
