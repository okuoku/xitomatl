#! /usr/bin/env scheme-script

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
(import
  (except (rnrs) file-exists?)
  (xitomatl irregex)
  (xitomatl match)
  (xitomatl enumerators)
  (xitomatl file-system base)
  (xitomatl file-system paths)
  (xitomatl common)
  (xitomatl define)
  (srfi :39 parameters))

(define format-mode (make-parameter 's-expr))

(define port-chunk-size (make-parameter 1024))  ;; good size?

(define interactive 
  (let ([prompt (lambda (str)
                  (display str)
                  (flush-output-port (current-output-port))
                  (get-line (current-input-port)))])
    (case-lambda
      [()
       (let ([irx (prompt "\nEnter regex: ")])
         (newline)
         (interactive irx))]
      [(irx)
       (set! irx (irregex irx 'fast))
       (let loop ()
         (let ([line (prompt "Enter line: ")])
           (if (eof-object? line)
             (newline)
             (let ([m (irregex-search irx line)])
               (if m
                 (let show ([n 0] [max (irregex-match-num-submatches m)])
                   (when (<= n max)
                     (printf "~a:\t~s\n" n (irregex-match-substring m n))
                     (show (+ 1 n) max)))
                 (display "No match.\n"))
               (loop)))))])))

(define (lines irx . files/dirs)
  ;;--------------------------------------------------------------------------
  (define (print-start/s-expr filename)
    (printf "(~s\n" filename))
  (define (print-end/s-expr _)
    (display ")\n"))
  (define (print-match/s-expr line line-number m)
    (printf " (~s ~s" line-number line)
    (let loop ([n 0] [max (irregex-match-num-submatches m)])
      (if (<= n max)
        (begin (printf "\n  (~s ~s ~s)" (irregex-match-substring m n)
                       (irregex-match-start-index m n) (irregex-match-end-index m n))
               (loop (+ 1 n) max))
        (printf ")\n"))))
  (define-values (print-start print-end print-match)
    (case (format-mode)
      [(s-expr)
       (values print-start/s-expr print-end/s-expr print-match/s-expr)]))
  ;;--------------------------------------------------------------------------
  (define (search port filename)
    (let loop ([count 0] [found #f])
      (let ([line (get-line port)])
        (if (eof-object? line)
          (when found
            (print-end filename))
          (let ([m (irregex-search irx line)])
            (cond [m
                   (unless found
                     (print-start filename))
                   (print-match line count m)
                   (loop (+ 1 count) #T)]
                  [else
                   (loop (+ 1 count) found)]))))))
  (define (search-file fn)
    (call-with-input-file fn (lambda (fip) (search fip fn))))
  ;;--------------------------------------------------------------------------
  (set! irx (irregex irx 'fast))
  (cond 
    [(null? files/dirs)
     (search (current-input-port) 'current-input-port)]
    [else
     (let-values ([(dirs files) (partition file-directory? files/dirs)])
       (for-each search-file
                 files)
       (for-each
        (lambda (dir) 
          (directory-walk (lambda (path dirs files syms)
                            (for-each (lambda (f)
                                        (search-file (path-join path f))) 
                                      files))
                          dir))
        dirs))]))

(define (single irx . files/dirs)
  ;;--------------------------------------------------------------------------
  (define (print-start/s-expr filename)
    (printf "(~s\n" filename))
  (define (print-end/s-expr _)
    (display ")\n"))
  (define (print-match/s-expr m)
    (let loop ([n 0] [max (irregex-match-num-submatches m)])
      (cond [(= n 0)
             (printf " (~s" (irregex-match-substring m n))
             (loop (+ 1 n) max)]
            [(<= n max)
             (printf "\n  ~s" (irregex-match-substring m n))
             (loop (+ 1 n) max)]
            [else
             (printf ")\n")])))
  (define-values (print-start print-end print-match)
    (case (format-mode)
      [(s-expr)
       (values print-start/s-expr print-end/s-expr print-match/s-expr)]))
  ;;--------------------------------------------------------------------------
  (define pe (irregex-port-enumerator (irregex irx 'single-line 'fast)
                                      (port-chunk-size)))
  (define (search port filename)
    (or (fold/enumerator pe port
                         (lambda (m first) 
                           (when first (print-start filename))
                           (print-match m)
                           (values #t #f))
                         #t)
        (print-end filename)))
  (define (search-file fn) 
    (call-with-input-file fn (lambda (fip) (search fip fn))))
  ;;--------------------------------------------------------------------------
  (cond 
    [(null? files/dirs)
     (search (current-input-port) 'current-input-port)]
    [else
     (let-values ([(dirs files) (partition file-directory? files/dirs)])
       (for-each search-file
                 files)
       (for-each 
        (lambda (dir) 
          (directory-walk (lambda (path dirs files syms)
                            (for-each (lambda (f)
                                        (search-file (path-join path f))) 
                                      files))
                          dir))
        dirs))]))

(define (print-help/exit)
  (define d display)
  (printf "Usage: ~a [command [options ...]]\n" (car (command-line)))
  (d " Commands:\n")
  (d "  (-i) --interactive [regex]       Prompt for lines to match against regex.\n")
  (d "                                    Prompt for regex if not supplied.\n")
  (d "  (-l) --lines regex [paths ...]   Search files, recursively descending into\n")
  (d "                                    directories, or (current-input-port),\n")
  (d "                                    for lines containing a match for regex.\n")
  (d "  (-s) --single regex [paths ...]  Search files, recursively descending into\n")
  (d "                                    directories, or (current-input-port),\n")
  (d "                                    across lines, with . matching newline.\n")
  (d " If no command is supplied, interactive is used.\n")
  (exit #f))

(define (main cmdln)
  (match cmdln
    [(_) 
     (main '(#f "--interactive"))]
    [(_ (:or "--interactive" "-i") args (... 0 1))
     (apply interactive args)]
    [(_ (:or "--lines" "-l") regex . args)
     (apply lines regex args)]
    [(_ (:or "--single" "-s") regex . args)
     (apply single regex args)]
    #;[(_ (:or "replace" "r") regex replacement . args)
     (apply replace args)]
    [_ 
     (print-help/exit)]))

(main (command-line))
