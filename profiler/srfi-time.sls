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
(library (xitomatl profiler srfi-time)
  (export
    case-lambda/profiled lambda/profiled define/profiled
    generate-report print-report)
  (import
    (rnrs)
    (srfi :19 time)
    (xitomatl profiler meta)
    (only (xitomatl enumerators) fold)
    (only (xitomatl common) fprintf pretty-print format))
  
  (define make-profiled-proxy 
    (make-make-profiled-proxy current-time add-duration 
                              (lambda (x y)
                                (if (eq? 'time-duration (time-type y))
                                  (subtract-duration x y)
                                  (time-difference x y)))))
  
  (def--case-lambda/profiled case-lambda/profiled make-profiled-proxy)
  
  (def--lambda/profiled lambda/profiled make-profiled-proxy)
  
  (def--define/profiled define/profiled make-profiled-proxy)
  
  (define (generate-report)
    (let-values ([(keys vals) (hashtable-entries (profiled-procedures-HT))])
      (vector->list vals)))
  
  (define print-report
    (case-lambda
      [()
       (print-report #f)]
      [(print-unused) 
       (print-report print-unused (generate-report))]
      [(print-unused report) 
       (print-report print-unused report (current-output-port))]
      [(print-unused report port)
       (define (fpf str . args) (apply fprintf port str args))
       (define (fpp x) (pretty-print x port))
       (define (fmt-num-set ns)
         (apply string-append (map (lambda (n) (format " ~a" n)) (list-sort < ns))))
       (for-each
        (lambda (pp)           
          (let-values 
              ([(calls-num returns-num entries/exits-num args-nums vals-nums times)
                (fold 
                 (profiled-procedure-uses pp)
                 (lambda (u c r e a v t)
                   (let ([start (procedure-use-start u)]
                         [stop (procedure-use-stop u)]
                         [called (procedure-use-called u)]
                         [returned (procedure-use-returned u)])
                     (values #T 
                             (if called (+ 1 c) c)
                             (if returned (+ 1 r) r)
                             (+ 1 e)
                             (if (and called (not (memv called a)))
                               (cons called a)
                               a)
                             (if (and returned (not (memv returned v)))
                               (cons returned v)
                               v)
                             (let* ([d (time-difference stop start)]
                                    [s (let ([s (time-second d)]
                                             [ns (/ (time-nanosecond d) #e1e9)])
                                         ((if (negative? s) - +) s ns))])
                               (cons (max s 0) t)))))
                 0 0 0 '() '() '())])
            (when (or (positive? entries/exits-num) print-unused)
              (fpf "\n=================================================================\n")
              (fpf "Profile for:\n")
              (fpp (profiled-procedure-source-code pp))
              (fpf "Statistics:\n")
              (fpf " calls: ~s   returns: ~s   entries/exits: ~s\n" 
                   calls-num returns-num entries/exits-num)
              (unless (null? args-nums)
                (fpf " numbers of arguments to calls:~a\n" (fmt-num-set args-nums)))
              (unless (null? vals-nums)
                (fpf " numbers of values returned:~a\n" (fmt-num-set vals-nums)))
              (unless (null? times)
                (let-values ([(count total minimum maximum)
                              (fold times
                                    (lambda (ti c to mi ma)
                                      (values #T 
                                              (+ 1 c)
                                              (+ ti to)
                                              (if mi (min ti mi) ti)
                                              (max ti ma)))
                                    0 0 #F 0)])
                  (fpf " average time: ~s sec\n" (inexact (/ total count)))
                  (fpf " minimum time: ~s sec\n" (inexact minimum))
                  (fpf " maximum time: ~s sec\n" (inexact maximum))))
              (fpf "=================================================================\n"))))
        report)]))

)
