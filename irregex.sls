;;;; irregex.scm -- IrRegular Expressions
;;
;; Copyright (c) 2005-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At this moment there was a loud ring at the bell, and I could
;; hear Mrs. Hudson, our landlady, raising her voice in a wail of
;; expostulation and dismay.
;;
;; "By heaven, Holmes," I said, half rising, "I believe that
;; they are really after us."
;;
;; "No, it's not quite so bad as that.  It is the unofficial
;; force, -- the Baker Street irregulars."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History
;;
;; 0.5: 2008/04/24 - fully portable R4RS, most of PCRE features implemented
;; 0.4: 2008/04/17 - rewriting NFA to use efficient closure compilation,
;;                   normal strings only, but all of the spencer tests pass
;; 0.3: 2008/03/10 - adding DFA converter (normal strings only)
;; 0.2: 2005/09/27 - adding irregex-opt (like elisp's regexp-opt) utility
;; 0.1: 2005/08/18 - simple NFA iterpreter over abstract chunked strings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turned into an R6RS library by Derick Eddington
;; TODO: use R6RS versions of find, filter, fold, etc
;; TODO: expand-time known-string optimization, ala Aziz's pregexp
;; TODO: follow Shinn's comments for optimizing
;; TODO: use case-lambda for optional arguments
;; TODO: irregex-search/all

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!r6rs
(library (xitomatl irregex (0 5))
  (export
    irregex string->irregex sre->irregex irregex?
    #;irregex-make-matches #;irregex-reset-matches!
    irregex-match-start irregex-match-end irregex-match-substring
    irregex-search irregex-search/matches irregex-match
    irregex-replace irregex-replace/all
    irregex-quote irregex-opt sre->string string->sre)
  (import
    (except (rnrs) find filter)
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (only (xitomatl strings) string-intersperse))

(define irregex-tag (list 'irregex))

(define (make-irregex dfa dfa/search dfa/extract nfa flags submatches lengths)
  (vector irregex-tag dfa dfa/search dfa/extract nfa flags submatches lengths))

(define (irregex? obj)
  (and (vector? obj)
       (= 8 (vector-length obj))
       (eq? irregex-tag (vector-ref obj 0))))

(define (irregex-dfa x) (vector-ref x 1))
(define (irregex-dfa/search x) (vector-ref x 2))
(define (irregex-dfa/extract x) (vector-ref x 3))
(define (irregex-nfa x) (vector-ref x 4))
(define (irregex-flags x) (vector-ref x 5))
(define (irregex-submatches x) (vector-ref x 6))
(define (irregex-lengths x) (vector-ref x 7))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error / exception reporting
  
(define (make-die who)
  (lambda (msg . irrts)
    (apply assertion-violation who msg irrts)))
  
(define-syntax define/die
  (lambda (stx)
    (syntax-case stx ()
      [(_ (who . frmls) . body)
       (with-syntax ([die (datum->syntax #'who 'die)])
         #'(define who 
             (let ([die (make-die 'who)])
               (lambda frmls . body))))])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string utilities

(define *min-char* #\x0)
(define *max-char* #\x10ffff)

(define (string-scan-char str c . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((eqv? c (string-ref str i)) i)
            (else (scan (+ i 1)))))))

(define (string-scan-char-escape str c . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((eqv? c (string-ref str i)) i)
            ((eqv? c #\\) (scan (+ i 2)))
            (else (scan (+ i 1)))))))

(define (string-split-char str c)
  (let ((end (string-length str)))
    (let lp ((i 0) (from 0) (res '()))
      (define (collect) (cons (substring str from i) res))
      (cond ((>= i end) (reverse (collect)))
            ((eqv? c (string-ref str i)) (lp (+ i 1) (+ i 1) (collect)))
            (else (lp (+ i 1) from res))))))

(define (char-alphanumeric? c)
  (or (char-alphabetic? c) (char-numeric? c)))

;; SRFI-13 extracts

(define (%%string-copy! to tstart from fstart fend)
  (do ((i fstart (+ i 1))
       (j tstart (+ j 1)))
      ((>= i fend))
    (string-set! to j (string-ref from i))))

(define (string-cat-reverse string-list)
  (string-cat-reverse/aux
   (fold (lambda (s a) (+ (string-length s) a)) 0 string-list)
   string-list))

(define (string-cat-reverse/aux len string-list)
  (let ((res (make-string len)))
    (let lp ((i len) (ls string-list))
      (if (pair? ls)
	  (let* ((s (car ls))
		 (slen (string-length s))
		 (i (- i slen)))
	    (%%string-copy! res i s 0 slen)
	    (lp i (cdr ls)))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list utilities

;; like the one-arg IOTA case
(define (zero-to n)
  (if (<= n 0)
      '()
      (let lp ((i (- n 1)) (res '()))
        (if (zero? i) (cons 0 res) (lp (- i 1) (cons i res))))))

;; take the head of list FROM up to but not including TO, which must
;; be a tail of the list
(define (take-up-to from to)
  (let lp ((ls from) (res '()))
    (if (and (pair? ls) (not (eq? ls to)))
        (lp (cdr ls) (cons (car ls) res))
        (reverse res))))

;; SRFI-1 extracts (simplified 1-ary versions)

(define (find pred ls)
  (cond ((find-tail pred ls) => car)
	(else #f)))

(define (find-tail pred ls)
  (let lp ((ls ls))
    (cond ((null? ls) #f)
          ((pred (car ls)) ls)
          (else (lp (cdr ls))))))

(define (last ls)
  (if (pair? (cdr ls))
      (last (cdr ls))
      ls))

(define (any pred ls)
  (and (pair? ls)
       (let lp ((head (car ls)) (tail (cdr ls)))
         (if (null? tail)
             (pred head)
             (or (pred head) (lp (car tail) (cdr tail)))))))

(define (every pred ls)
  (or (null? ls)
      (let lp ((head (car ls))  (tail (cdr ls)))
        (if (null? tail)
            (pred head)
            (and (pred head) (lp (car tail) (cdr tail)))))))

(define (fold kons knil ls)
  (let lp ((ls ls) (res knil))
    (if (null? ls)
        res
        (lp (cdr ls) (kons (car ls) res)))))

(define (filter pred ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cdr ls) (if (pred (car ls)) (cons (car ls) res) res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flags (bitwise operators would be simpler and faster)

(define (flag-set? flags i)
  (odd? (quotient flags (expt 2 i))))
(define (flag-join a b)
  (if (not b)
      a
      (let* ((pow (expt 2 b))
             (tmp (quotient a pow))
             (rem (modulo a pow))
             (tmp (if (even? tmp) (+ tmp 1) tmp)))
        (+ (* tmp pow) rem))))
(define (flag-clear a b)
  (if (not b)
      a
      (let* ((pow (expt 2 b))
             (tmp (quotient a pow))
             (rem (modulo a pow))
             (tmp (if (odd? tmp) (- tmp 1) tmp)))
        (+ (* tmp pow) rem))))

(define ~none 0)
(define ~searcher? 1)
(define ~consumer? 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing

(define ~save? 1)
(define ~case-insensitive? 2)
(define ~multi-line? 3)
(define ~single-line? 4)
(define ~ignore-space? 5)
(define ~utf8? 6)

(define (symbol-list->flags ls)
  (let lp ((ls ls) (res ~none))
    (if (not (pair? ls))
        res
        (lp (cdr ls)
            (flag-join
             res
             (case (car ls)
               ((i ci case-insensitive) ~case-insensitive?)
               ((m multi-line) ~multi-line?)
               ((s single-line) ~single-line?)
               ((x ignore-space) ~ignore-space?)
               ((u utf8) ~utf8?)
               (else #f)))))))

(define/die (string->sre str . o)  
  (let ((end (string-length str))
        (flags (symbol-list->flags o)))

    (let lp ((i 0) (from 0) (flags flags) (res '()) (stack '()))

      ;; handle case sensitivity at the literal char/string level
      (define (cased-char ch)
        (if (and (flag-set? flags ~case-insensitive?)
                 (char-alphabetic? ch))
            `(or ,ch ,(char-altcase ch))
            ch))
      (define (cased-string str)
        (if (flag-set? flags ~case-insensitive?)
            (sre-sequence (map cased-char (string->list str)))
            str))
      ;; accumulate the substring from..i as literal text
      (define (collect)
        (if (= i from) res (cons (cased-string (substring str from i)) res)))
      ;; like collect but breaks off the last single character when
      ;; collecting literal data, as the argument to ?/*/+ etc.
      (define (collect/single)
        (cond ((= i from) res)
              ((= i (+ from 1)) (cons (cased-char (string-ref str from)) res))
              (else
               (cons (cased-char (string-ref str (- i 1)))
                     (cons (cased-string (substring str from (- i 1))) res)))))
      ;; collects for use as a result, reversing and grouping OR terms
      (define (collect/terms)
        (let lp ((ls (collect)) (term '()) (res '()))
          (define (shift) (cons (sre-sequence term) res))
          (cond
           ((null? ls)
            (let ((res (sre-alternate (shift))))
              (if (flag-set? flags ~save?) (list 'submatch res) res)))
           ((eq? 'or (car ls)) (lp (cdr ls) '() (shift)))
           (else (lp (cdr ls) (cons (car ls) term) res)))))

      ;; main parsing
      (if (>= i end)
          (if (pair? stack)
              (die "unterminated parenthesis in regexp" str)
              (collect/terms))
          (let ((c (string-ref str i)))
            (case c
              ((#\.)
               (let ((sym (if (flag-set? flags ~multi-line?) 'nonl 'any)))
                 (lp (+ i 1) (+ i 1) flags (cons sym (collect)) stack)))
              ((#\?)
               (let* ((res (collect/single))
                      (x (car res)))
                 (lp (+ i 1)
                     (+ i 1)
                     flags
                     (cons
                      (if (pair? x)
                          (case (car x)
                            ((*)  `(*? ,@(cdr x)))
                            ((+)  `(**? 1 #f ,@(cdr x)))
                            ((?)  `(?? ,@(cdr x)))
                            ((**) `(**? ,@(cdr x)))
                            (else `(? ,x)))
                          `(? ,x))
                      (cdr res))
                     stack)))
              ((#\+ #\*)
               (let* ((res (collect/single))
                      (x (car res))
                      (op (string->symbol (string c))))
                 (cond
                  ((sre-repeater? x)
                   (die "duplicate repetition (e.g. **) in sre" str res))
                  ((sre-empty? x)
                   (die "can't repeat empty sre (e.g. ()*)" str res))
                  (else
                   (lp (+ i 1) (+ i 1) flags
                       (cons (list op x) (cdr res))
                       stack)))))
              ((#\()
               (if (eqv? #\? (string-ref str (+ i 1)))
                   (case (string-ref str (+ i 2))
                     ((#\#)
                      (let ((j (string-scan-char str #\) (+ i 3))))
                        (lp (+ j i) (+ j 1) flags (collect) stack)))
                     ((#\:)
                      (lp (+ i 3) (+ i 3) flags '()
                          (cons (cons flags (collect)) stack)))
                     ((#\= #\! #\< #\> #\( #\{)
                      (die "unsupported Perl-style cluster" str))
                     (else
                      (let ((old-flags flags))
                        (let lp2 ((j (+ i 2)) (flags flags) (invert? #f))
                          (define (join x)
                            ((if invert? flag-clear flag-join) flags x))
                          (case (string-ref str j)
                            ((#\i)
                             (lp2 (+ j 1) (join ~case-insensitive?) invert?))
                            ((#\m)
                             (lp2 (+ j 1) (join ~multi-line?) invert?))
                            ((#\x)
                             (lp2 (+ j 1) (join ~ignore-space?) invert?))
                            ((#\-)
                             (lp2 (+ j 1) flags (not invert?)))
                            ((#\))
                             (lp (+ j 1) (+ j 1) flags (collect) stack))
                            ((#\:)
                             (lp (+ j 1) (+ j 1) flags '()
                                 (cons (cons old-flags (collect)) stack)))
                            (else
                             (die "unknown regex cluster modifier" str)))))))
                   (lp (+ i 1) (+ i 1)
                       (flag-join flags ~save?)
                       '()
                       (cons (cons flags (collect)) stack))))
              ((#\))
               (if (null? stack)
                   (die "too many )'s in regexp" str)
                   (lp (+ i 1)
                       (+ i 1)
                       (caar stack)
                       (cons (collect/terms) (cdar stack))
                       (cdr stack))))
              ((#\[)
               (cond
                ((>= (+ i 1) end)
                 (die "unterminated char set" str))
                ((eqv? #\: (string-ref str (+ i 1)))
                 (let ((j (string-scan-char str #\: (+ i 2))))
                   (if (or (not j)
                           (>= j end)
                           (not (eqv? #\] (string-ref str (+ j 1)))))
                       (die "unterminated POSIX class in char set" str)
                       (lp (+ j 2) (+ j 2) flags
                           (cons (string->symbol (substring str (+ i 2) j))
                                 (collect))
                           stack))))
                (else
                 (let* ((i2 (if (eqv? #\^ (string-ref str (+ i 1)))
                                (+ i 3)
                                (+ i 2)))
                        (j (string-scan-char-escape str #\] i2)))
                   (if (not j)
                       (die "unterminated char set" str)
                       (lp (+ j 1)
                           (+ j 1)
                           flags
                           (cons (posix-char-set->sre
                                  (substring str (+ i 1) j)
                                  flags)
                                 (collect))
                           stack))))))
              ((#\{)
               (let* ((res (collect/single))
                      (x (car res))
                      (tail (cdr res))
                      (j (string-scan-char str #\} (+ i 1)))
                      (s2 (string-split-char (substring str (+ i 1) j) #\,))
                      (n (or (string->number (car s2)) 0))
                      (m (and (pair? (cdr s2)) (string->number (cadr s2)))))
                 (cond
                  ((null? (cdr s2))
                   (lp (+ j 1) (+ j 1) flags (cons (list '= n x) tail) '()))
                  (m
                   (lp (+ j 1) (+ j 1) flags (cons (list '** n m x) tail) '()))
                  (else
                   (lp (+ j 1) (+ j 1) flags (cons (list '>= n x) tail) '())))))
              ((#\\)
               (let ((c (string-ref str (+ i 1))))
                 (case c
                   ((#\d)
                    (lp (+ i 2) (+ i 2) flags `(numeric ,@(collect)) stack))
                   ((#\D)
                    (lp (+ i 2) (+ i 2) flags `((~ numeric) ,@(collect)) stack))
                   ((#\s)
                    (lp (+ i 2) (+ i 2) flags `(space ,@(collect)) stack))
                   ((#\S)
                    (lp (+ i 2) (+ i 2) flags `((~ space) ,@(collect)) stack))
                   ((#\w)
                    (lp (+ i 2) (+ i 2) flags
                        `((or alphanumeric ("_")) ,@(collect)) stack))
                   ((#\W)
                    (lp (+ i 2) (+ i 2) flags
                        `((~ (or alphanumeric ("_"))) ,@(collect)) stack))
                   ((#\b)
                    (lp (+ i 2) (+ i 2) flags
                        `((or bow eow) ,@(collect)) stack))
                   ((#\A)
                    (lp (+ i 2) (+ i 2) flags `(bos ,@(collect)) stack))
                   ((#\Z)
                    (lp (+ i 2) (+ i 2) flags
                        `((? #\newline) eos ,@(collect)) stack))
                   ((#\z)
                    (lp (+ i 2) (+ i 2) flags `(eos ,@(collect)) stack))
                   (else
                    (cond
                     ((char-numeric? c)
                      (let ((res `((backref ,(string->number (string c)))
                                   ,@(collect))))
                        (lp (+ i 2) (+ i 2) flags res stack)))
                     ((char-alphabetic? c)
                      (die "unknown escape sequence" str c))
                     (else
                      (lp (+ i 2) (+ i 1) flags (collect) stack)))))))
              ((#\|)
               (lp (+ i 1) (+ i 1) flags (cons 'or (collect)) stack))
              ((#\^)
               (let ((sym (if (flag-set? flags ~multi-line?) 'bol 'bos)))
                 (lp (+ i 1) (+ i 1) flags (cons sym (collect)) stack)))
              ((#\$)
               (let ((sym (if (flag-set? flags ~multi-line?) 'eol 'eos)))
                 (lp (+ i 1) (+ i 1) flags (cons sym (collect)) stack)))
              ((#\space)
               (if (flag-set? flags ~ignore-space?)
                   (lp (+ i 1) (+ i 1) flags (collect) stack)
                   (lp (+ i 1) from flags res stack)))
              ((#\#)
               (let ((j (or (string-scan-char str #\newline (+ i 1))
                            (- end 1))))
                 (lp (+ j 1) (+ j 1) flags (collect) stack)))
              (else
               (lp (+ i 1) from flags res stack))))))))

(define (char-altcase c)
  (if (char-upper-case? c) (char-downcase c) (char-upcase c)))

(define/die (posix-char-set->sre str flags)
  (let ((end (string-length str)))
    (define (expand-char-cases ls)
      (let lp ((ls ls) (res '()))
        (if (null? ls)
            res
            (let* ((c1 (car ls))
                   (c2 (char-altcase c1)))
              (lp (cdr ls) (if (eqv? c1 c2) `(,c1 ,@res) `(,c2 ,c1 ,@res)))))))
    (define (expand-char-range-cases ls)
      (let lp ((ls ls) (res '()))
        (if (null? ls)
            res
            (let* ((c1 (car ls))
                   (c2 (char-altcase c1))
                   (c3 (cadr ls))
                   (c4 (char-altcase c3)))
              (lp (cddr ls)
                  (if (or (eqv? c1 c2) (eqv? c3 c4))
                      `(,c1 ,c3 ,@res)
                      `(,c2 ,c4 ,c1 ,c3 ,@res)))))))
    (define (go i chars ranges)
      (if (= i end)
          (let ((ci? (flag-set? flags ~case-insensitive?)))
            (append
             (if (pair? chars)
                 (let ((chars (if ci? (expand-char-cases chars) chars)))
                   (list (list (list->string (reverse chars)))))
                 '())
             (if (pair? ranges)
                 (let ((ranges
                        (if ci? (expand-char-range-cases ranges) ranges)))
                   (list (cons '/ (reverse ranges))))
                 '())))
          (let ((c (string-ref str i)))
            (case c
              ((#\-)
               (cond
                ((or (= i 0)
                     (and (= i 1) (eqv? #\^ (string-ref str 0)))
                     (= i (- end 1)))
                 (go (+ i 1) (cons c chars) ranges))
                ((null? chars)
                 (die "bad char-set"))
                (else
                 (let ((c1 (car chars))
                       (c2 (string-ref str (+ i 1))))
                   (if (char<? c2 c1)
                       (die "inverted range in char-set" c1 c2)
                       (go (+ i 2) (cdr chars) (cons c2 (cons c1 ranges))))))))
              ((#\\)
               (go (+ i 2)
                   (cons (string-ref str (+ i 1)) (cons c chars))
                   ranges))
              (else
               (go (+ i 1) (cons c chars) ranges))))))
    (if (eqv? #\^ (string-ref str 0))
        (let ((chars (if (flag-set? flags ~multi-line?) '(#\newline) '())))
          (cons '~ (go 1 chars '())))
        (sre-alternate (go 0 '() '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structures

(define (irregex x . o)
  (cond
   ((irregex? x) x)
   ((string? x) (apply string->irregex x o))
   (else (sre->irregex (sre-adjust-cases x (memq 'case-insensitive o)) o))))

(define (string->irregex str . o)
  (apply sre->irregex (apply string->sre str o) o))

(define (sre->irregex sre . o)
  (if (memq 'small o)
      (make-irregex #f #f #f (sre->procedure sre) ~none
                    (sre-count-submatchs sre) (sre-length-ranges sre))
      (let* ((searcher? (sre-searcher? sre))
             (dfa (cond ((sre->nfa sre) => nfa->dfa) (else #f)))
             (dfa/search
              (and dfa
                   (not searcher?)
                   (cond ((sre->nfa `(seq (* any) ,sre)) => nfa->dfa)
                         (else #f))))
             (extractor (and dfa (sre-match-extractor sre)))
             (submatches (sre-count-submatchs sre))
             (lengths (sre-length-ranges sre))
             (flags (flag-join
                     (flag-join ~none (and searcher? ~searcher?))
                     (and (sre-consumer? sre) ~consumer?))))
        (cond
         (dfa
          (make-irregex dfa dfa/search extractor #f flags submatches lengths))
         (else
          (make-irregex #f #f #f (sre->procedure sre) flags submatches lengths)
          )))))

(define (irregex-make-matches irx)
  (make-vector (* 2 (+ 1 (irregex-submatches irx))) #f))
(define (irregex-reset-matches! m)
  (vector-fill! m #f)
  m)

;; inline these
(define (%irregex-match-start m n)
  (vector-ref m (* n 2)))
(define (%irregex-match-end m n)
  (vector-ref m (+ 1 (* n 2))))

(define (%irregex-match-start-set! m n start)
  (vector-set! m (* n 2) start))
(define (%irregex-match-end-set! m n end)
  (vector-set! m (+ 1 (* n 2)) end))

(define (irregex-match-substring m str n)
  (and (< (* n 2) (vector-length m))
       (vector-ref m (+ 1 (* n 2)))
       (substring str (vector-ref m (* n 2)) (vector-ref m (+ 1 (* n 2))))))
(define (irregex-match-start m str n)
  (and (< (* n 2) (vector-length m))
       (vector-ref m (+ 1 (* n 2)))
       (vector-ref m (* n 2))))
(define (irregex-match-end m str n)
  (and (< (* n 2) (vector-length m))
       (vector-ref m (+ 1 (* n 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sre analysis

;; returns #t if the sre can ever be empty
(define (sre-empty? sre)
  (if (pair? sre)
      (case (car sre)
        ((* ?) #t)
        ((or) (any sre-empty? (cdr sre)))
        ((: seq submatch +) (every sre-empty? (cdr sre)))
        (else #f))
      (memq sre '(epsilon bos eos bol eol bow eow))))

(define (sre-any? sre)
  (or (eq? sre 'any)
      (and (pair? sre)
           (case (car sre)
             ((seq : submatch)
              (and (pair? (cdr sre)) (null? (cddr sre)) (sre-any? (cadr sre))))
             ((or) (every sre-any? (cdr sre)))
             (else #f)))))

(define (sre-repeater? sre)
  (and (pair? sre)
       (or (memq (car sre) '(* +))
           (and (memq (car sre) '(submatch seq :))
                (pair? (cdr sre))
                (null? (cddr sre))
                (sre-repeater? (cadr sre))))))

(define (sre-searcher? sre)
  (if (pair? sre)
      (case (car sre)
        ((* +) (sre-any? (sre-sequence (cdr sre))))
        ((seq : submatch) (and (pair? (cdr sre)) (sre-searcher? (cadr sre))))
        ((or) (every sre-searcher? (cdr sre)))
        (else #f))
      (eq? 'bos sre)))

(define (sre-consumer? sre)
  (if (pair? sre)
      (case (car sre)
        ((* +) (sre-any? (sre-sequence (cdr sre))))
        ((seq : submatch) (and (pair? (cdr sre)) (sre-consumer? (last sre))))
        ((or) (every sre-consumer? (cdr sre)))
        (else #f))
      (eq? 'eos sre)))

(define (sre-has-submatchs? sre)
  (and (pair? sre)
       (or (eq? 'submatch (car sre))
           (any sre-has-submatchs? (cdr sre)))))

(define (sre-count-submatchs sre)
  (let count ((sre sre) (sum 0))
    (if (pair? sre)
        (fold count
              (+ sum (case (car sre)
                       ((submatch) 1)
                       ((dsm) (+ (cadr sre) (caddr sre)))
                       (else 0)))
              (cdr sre))
        sum)))

(define/die (sre-length-ranges sre)
  (let ((sublens (make-vector (+ 1 (sre-count-submatchs sre)) #f)))
    (vector-set!
     sublens
     0
     (let lp ((sre sre) (n 1) (lo 0) (hi 0) (return cons))
       (define (grow i) (return (+ lo i) (and hi (+ hi i))))
       (cond
        ((pair? sre)
         (if (string? (car sre))
             (grow 1)
             (case (car sre)
               ((/ ~ & -)
                (grow 1))
               ((seq : w/case w/nocase)
                (let lp2 ((ls (cdr sre)) (n n) (lo2 0) (hi2 0))
                  (if (null? ls)
                      (return (+ lo lo2) (and hi hi2 (+ hi hi2)))
                      (lp (car ls) n 0 0
                          (lambda (lo3 hi3)
                            (lp2 (cdr ls)
                                 (+ n (sre-count-submatchs (car sre)))
                                 (+ lo2 lo3)
                                 (and hi2 hi3 (+ hi2 hi3))))))))
               ((or)
                (let lp2 ((ls (cdr sre)) (n n) (lo2 #f) (hi2 0))
                  (if (null? ls)
                      (return (+ lo lo2) (and hi hi2 (+ hi hi2)))
                      (lp (car ls) n 0 0
                          (lambda (lo3 hi3)
                            (lp2 (cdr ls)
                                 (+ n (sre-count-submatchs (car sre)))
                                 (if lo2 (min lo2 lo3) lo3)
                                 (and hi2 hi3 (max hi2 hi3))))))))
               ((dsm)
                (lp (sre-sequence (cdddr sre)) (+ n (cadr sre)) lo hi return))
               ((submatch)
                (lp (sre-sequence (cdr sre)) (+ n 1) lo hi
                    (lambda (lo2 hi2)
                      (vector-set! sublens n (cons lo2 hi2))
                      (return lo2 hi2))))
               ((backref)
                (let ((n (cadr sre)))
                  (cond
                   ((or (not (integer? n))
                        (not (< 0 n (vector-length sublens))))
                    (die "sre-length: invalid backreference" sre))
                   ((not (vector-ref sublens n))
                    (die "sre-length: invalid forward backreference" sre))
                   (else
                    (let ((lo2 (car (vector-ref sublens n)))
                          (hi2 (cdr (vector-ref sublens n))))
                      (return (+ lo lo2) (and hi hi2 (+ hi hi2))))))))
               ((* *?)
                (lp (sre-sequence (cdr sre)) n lo hi (lambda (lo hi) #f))
                (return lo #f))
               ((** **?)
                (cond
                 ((or (and (number? (cadr sre))
                           (number? (caddr sre))
                           (> (cadr sre) (caddr sre)))
                      (and (not (cadr sre)) (caddr sre)))
                  (return lo hi))
                 (else
                  (if (caddr sre)
                      (lp (sre-sequence (cdddr sre)) n 0 0
                          (lambda (lo2 hi2)
                            (return (+ lo (* (cadr sre) lo2))
                                    (and hi hi2 (+ hi (* (caddr sre) hi2))))))
                      (lp (sre-sequence (cdddr sre)) n 0 0
                          (lambda (lo2 hi2)
                            (return (+ lo (* (cadr sre) lo2)) #f)))))))
               ((+)
                (lp (sre-sequence (cdr sre)) n lo hi
                    (lambda (lo2 hi2)
                      (return (+ lo lo2) #f))))
               ((? ??)
                (lp (sre-sequence (cdr sre)) n lo hi
                    (lambda (lo2 hi2)
                      (return lo (and hi hi2 (+ hi hi2))))))
               ((= =? >= >=?)
                (lp `(** ,(cadr sre)
                         ,(if (memq (car sre) '(>= >=?)) #f (cadr sre))
                         ,@(cddr sre))
                    n lo hi return))
               (else
                (die "sre-length: unknown sre operator" sre)))))
        ((char? sre)
         (grow 1))
        ((string? sre)
         (grow (string-length sre)))
        ((memq sre '(any nonl))
         (grow 1))
        ((memq sre '(epsilon bos eos bol eol bow eow))
         (return lo hi))
        (else
         (let ((cell (assq sre sre-named-definitions)))
           (if cell
               (lp (cdr cell) n lo hi return)
               (die "sre-length: unknown sre" sre)))))))
    sublens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sre manipulation

;; build a (seq ls ...) sre from a list
(define (sre-sequence ls)
  (cond
   ((null? ls) 'epsilon)
   ((null? (cdr ls)) (car ls))
   (else (cons 'seq ls))))

;; build a (or ls ...) sre from a list
(define (sre-alternate ls)
  (cond
   ((null? ls) 'epsilon)
   ((null? (cdr ls)) (car ls))
   (else (cons 'or ls))))

;; returns an equivalent SRE without any match information
(define (sre-strip-submatches sre)
  (if (not (pair? sre))
      sre
      (case (car sre)
        ((submatch) (sre-strip-submatches (sre-sequence (cdr sre))))
        ((dsm) (sre-strip-submatches (sre-sequence (cdddr sre))))
        (else (map sre-strip-submatches sre)))))

;; given a char-set list of chars and strings, flattens them into
;; chars only
(define (sre-flatten-ranges ls)
  (let lp ((ls ls) (res '()))
    (cond
     ((null? ls)
      (reverse res))
     ((char? (car ls))
      (lp (cdr ls) (cons (car ls) res)))
     (else
      (lp (append (string->list (car ls)) (cdr ls)) res)))))

(define (sre-adjust-cases sre . o)
  (let lp ((sre sre) (ci? (and (pair? o) (car o))))
    (cond ((char? sre)
           (if (and ci? (char-alphabetic? sre))
               `(or ,sre ,(char-altcase sre))
               sre))
          ((string? sre)
           (if ci?
               (sre-sequence
                (map (lambda (c)
                       (if (char-alphabetic? c)
                           `(or ,c ,(char-altcase c))
                           c))
                     (string->list sre)))
               sre))
          ((pair? sre)
           (cond
            ((string? (car sre))
             (if ci? (cset->sre (sre->cset sre #t)) sre))
            (else
             (case (car sre)
               ((w/case) (sre-sequence (map (lambda (x) (lp x #f)) (cdr sre))))
               ((w/nocase) (sre-sequence (map (lambda (x) (lp x #t)) (cdr sre))))
               ((~ / - &) (if ci? (cset->sre (sre->cset sre #t)) sre))
               (else (map (lambda (x) (lp x ci?)) sre))))))
          (else sre))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matching

(define (irregex-search x str . o)
  (let ((irx (irregex x)))
    (let ((start (if (pair? o) (car o) 0))
          (end (if (and (pair? o) (pair? (cdr o)))
                   (cadr o)
                   (string-length str)))
          (matches (irregex-make-matches irx)))
      (irregex-search/matches irx str start end matches))))

;; internal routine, can be used in loops to avoid reallocating the
;; match vector
(define (irregex-search/matches irx str start end matches)
  (cond
   ((irregex-dfa irx)
    (cond
     ((flag-set? (irregex-flags irx) ~searcher?)
      (let ((m-end (dfa-match/longest (irregex-dfa irx) str start end)))
        (cond
         (m-end
          (%irregex-match-start-set! matches 0 start)
          (%irregex-match-end-set! matches 0 m-end)
          ((irregex-dfa/extract irx) str start m-end matches)
          matches)
         (else
          #f))))
     (else
      (let ((first-match
             (dfa-match/shortest (irregex-dfa/search irx) str start end)))
        (and
         first-match
         (let* ((lo+hi (vector-ref (irregex-lengths irx) 0))
                (m-start (if (cdr lo+hi)
                             (max start (- first-match (cdr lo+hi)))
                             start))
                (m-limit (- first-match (car lo+hi)))
                (dfa (irregex-dfa irx)))
           (let lp ((m-start m-start))
             (and (<= m-start m-limit)
                  (let ((m-end (dfa-match/longest dfa str m-start end)))
                    (cond
                     (m-end
                      (%irregex-match-start-set! matches 0 m-start)
                      (%irregex-match-end-set! matches 0 m-end)
                      ((irregex-dfa/extract irx) str m-start m-end matches)
                      matches)
                     (else
                      (lp (+ m-start 1)))))))))))))
   (else
    (let ((matcher (irregex-nfa irx)))
      (let lp ((start start))
        (and (<= start end)
             (let ((i (matcher str start matches (lambda () #f))))
               (cond
                (i
                 (%irregex-match-start-set! matches 0 start)
                 (%irregex-match-end-set! matches 0 i)
                 matches)
                (else
                 (lp (+ start 1)))))))))))

(define (irregex-match irx str)
  (let* ((irx (irregex irx))
         (matches (irregex-make-matches irx))
         (start 0)
         (end (string-length str)))
    (cond
     ((irregex-dfa irx)
      (let ((m-end (dfa-match/longest (irregex-dfa irx) str start end)))
        (cond
         (m-end
          (%irregex-match-start-set! matches 0 start)
          (%irregex-match-end-set! matches 0 m-end)
          ((irregex-dfa/extract irx) str start m-end matches)
          matches)
         (else
          #f))))
     (else
      (let* ((matcher (irregex-nfa irx))
             (i (matcher str start matches (lambda () #f))))
        (cond
         (i
          (%irregex-match-start-set! matches 0 start)
          (%irregex-match-end-set! matches 0 i)
          matches)
         (else
          #f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DFA matching

;; inline these
(define (dfa-init-state dfa)
  (vector-ref dfa 0))
(define (dfa-next-state dfa node)
  (vector-ref dfa (cdr node)))
(define (dfa-final-state? dfa state)
  (car state))

;; this searches for the first end index for which a match is possible
(define (dfa-match/shortest dfa str start end)
  (let lp ((i start) (state (dfa-init-state dfa)))
    (if (dfa-final-state? dfa state)
        i
        (and (< i end)
             (let* ((ch (string-ref str i))
                    (next (find (lambda (x)
                                  (or (eqv? ch (car x))
                                      (and (pair? (car x))
                                           (char<=? (caar x) ch)
                                           (char<=? ch (cdar x)))))
                                (cdr state))))
               (and next (lp (+ i 1) (dfa-next-state dfa next))))))))

;; this finds the longest match starting at a given index
(define (dfa-match/longest dfa str start end)
  (let lp ((i start)
           (state (dfa-init-state dfa))
           (res (and (dfa-final-state? dfa (dfa-init-state dfa)) start)))
    (if (>= i end)
        res
        (let* ((ch (string-ref str i))
               (cell (find (lambda (x)
                             (or (eqv? ch (car x))
                                 (and (pair? (car x))
                                      (char<=? (caar x) ch)
                                      (char<=? ch (cdar x)))))
                           (cdr state))))
          (if cell
              (let ((next (dfa-next-state dfa cell)))
                (lp (+ i 1)
                    next
                    (if (dfa-final-state? dfa next) (+ i 1) res)))
              res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SRE->NFA compilation
;;
;; An NFA state is a numbered node with a list of patter->number
;; transitions, where pattern is either a character, (lo . hi)
;; character range, or epsilon (indicating an empty transition).
;; There may be duplicate characters and overlapping ranges - since
;; it's an NFA we process it by considering all possible transitions.

(define sre-named-definitions
  `((any / ,*min-char* ,*max-char*)
    (nonl / ,*min-char* ,(integer->char 9) ,(integer->char 11) ,*max-char*)
    (alphabetic / #\a #\z #\A #\Z)
    (alpha . alphabetic)
    (alphanumeric / #\a #\z #\A #\Z #\0 #\9)
    (alphanum . alphanumeric)
    (alnum . alphanumeric)
    (lower-case / #\a #\z)
    (lower . lower-case)
    (upper-case / #\A #\Z)
    (upper . upper-case)
    (numeric / #\0 #\9)
    (num . numeric)
    (digit . numeric)
    (punctuation or #\! #\" #\# #\% #\& #\' #\( #\) #\* #\, #\- #\.
                    #\/ #\: #\; #\? #\@ #\[ #\\ #\] #\_ #\{ #\})
    (punct . punctuation)
    (graphic or alphanumeric punctuation #\$ #\+ #\< #\= #\> #\^ #\` #\| #\~)
    (graph . graphic)
    (blank or #\space ,(integer->char 9))
    (whitespace or #\space #\newline ,(integer->char 9))
    (space . whitespace)
    (white . whitespace)
    (printing or graphic whitespace)
    (print . printing)
    (control / ,*min-char* ,(integer->char 31))
    (cntrl . control)
    (hex-digit or numeric (/ #\a #\f #\A #\F))
    (xdigit . hex-digit)
    (ascii / ,*min-char* ,(integer->char 127))
    ))

;; Compile and return the list of NFA states.  The start state will be
;; at the head of the list, and all remaining states will be in
;; descending numeric order, with state 0 being the unique accepting
;; state.
(define (sre->nfa sre)
  ;; we loop over an implicit sequence list
  (let lp ((ls (list sre)) (n 1) (next (list (list 0))))
    (define (new-state-number state)
      (max n (+ 1 (caar state))))
    (define (extend-state next . trans)
      (and next
           (cons (cons (new-state-number next)
                       (map (lambda (x) (cons x (caar next))) trans))
                 next)))
    (if (null? ls)
        next
        (cond
         ((string? (car ls))
          ;; process literal strings a char at a time
          (lp (append (string->list (car ls)) (cdr ls)) n next))
         ((or (char? (car ls)) (eq? 'epsilon (car ls)))
          ;; chars and epsilons go directly into the transition table
          (extend-state (lp (cdr ls) n next) (car ls)))
         ((symbol? (car ls))
          (let ((cell (assq (car ls) sre-named-definitions)))
            (and cell (lp (cons (cdr cell) (cdr ls)) n next))))
         ((pair? (car ls))
          (cond
           ((string? (caar ls))
            ;; enumerated character set
            (lp (cons (sre-alternate (string->list (caar ls))) (cdr ls))
                n
                next))
           (else
            (case (caar ls)
              ((seq : w/case w/nocase)
               ;; for an explicit sequence, just append to the list
               (lp (append (cdar ls) (cdr ls)) n next))
              ((/ - & ~)
               (let ((ranges (sre->cset (car ls))))
                 (case (length ranges)
                   ((1)
                    (extend-state (lp (cdr ls) n next) (car ranges)))
                   (else
                    (lp (cons
                         (sre-alternate
                          (map (lambda (x) (list '/ (car x) (cdr x))) ranges))
                         (cdr ls))
                        n
                        next)))))
              ((or)
               (let* ((next (lp (cdr ls) n next))
                      (b (and next
                              (lp (list (sre-alternate (cddar ls)))
                                  (new-state-number next)
                                  next)))
                      (a (and b (lp (list (cadar ls))
                                    (new-state-number b)
                                    next))))
                 ;; compile both branches and insert epsilon
                 ;; transitions to either
                 (and a
                      `((,(new-state-number a)
                         (epsilon . ,(caar a))
                         (epsilon . ,(caar b)))
                        ,@(take-up-to a next)
                        ,@b))))
              ((?)
               (let ((next (lp (cdr ls) n next)))
                 ;; insert an epsilon transition directly to next
                 (and
                  next
                  (let ((a (lp (cdar ls) (new-state-number next) next)))
                    (set-cdr! (car a) `((epsilon . ,(caar next)) ,@(cdar a)))
                    a))))
              ((+ *)
               (let ((next (lp (cdr ls) n next)))
                 (and
                  next
                  (let* ((new (lp '(epsilon) (new-state-number next) next))
                         (a (lp (cdar ls) (new-state-number new) new)))
                    (and
                     a
                     (begin
                       ;; for *, insert an epsilon transition as in ? above
                       (if (eq? '* (caar ls))
                           (set-cdr! (car a)
                                     `((epsilon . ,(caar new)) ,@(cdar a))))
                       ;; for both, insert a loop back to self
                       (set-cdr! (car new)
                                 `((epsilon . ,(caar a)) ,@(cdar new)))
                       a))))))
              ((submatch)
               ;; ignore submatches altogether
               (lp (cons (sre-sequence (cdar ls)) (cdr ls)) n next))
              (else
               #f)))))
         (else
          #f)))))

;; We don't really want to use this, we use the closure compilation
;; below instead, but this is included for reference and testing the
;; sre->nfa conversion.

;; (define (nfa-match nfa str)
;;   (let lp ((ls (string->list str)) (state (car nfa)) (epsilons '()))
;;     (if (null? ls)
;;         (zero? (car state))
;;         (any (lambda (m)
;;                (if (eq? 'epsilon (car m))
;;                    (and (not (memv (cdr m) epsilons))
;;                         (lp ls (assv (cdr m) nfa) (cons (cdr m) epsilons)))
;;                    (and (or (eqv? (car m) (car ls))
;;                             (and (pair? (car m))
;;                                  (char<=? (caar m) (car ls))
;;                                  (char<=? (car ls) (cdar m))))
;;                         (lp (cdr ls) (assv (cdr m) nfa) '()))))
;;              (cdr state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NFA->DFA compilation
;;
;; During processing, the DFA is a list of the form:
;;
;;   ((NFA-states ...) accepting-state? transitions ...)
;;
;; where the transitions are as in the NFA, except there are no
;; epsilons, duplicate characters or overlapping char-set ranges, and
;; the states moved to are closures (sets of NFA states).  Multiple
;; DFA states may be accepting states.

(define (nfa->dfa nfa)
  (let lp ((ls (list (nfa-closure nfa (list (caar nfa)))))
           (res '()))
    (cond
     ((null? ls)
      (dfa-renumber (reverse res)))
     ((assoc (car ls) res)
      (lp (cdr ls) res))
     (else
      (let* ((states (car ls))
             (trans (nfa-state-transitions nfa states))
             (accept? (and (memv 0 states) #t)))
        (lp (append (map cdr trans) (cdr ls))
            `((,states ,accept? ,@trans) ,@res)))))))

;; When the conversion is complete we renumber the DFA sets-of-states
;; in order and convert the result to a vector for fast lookup.
(define (dfa-renumber dfa)
  (let ((states (map cons (map car dfa) (zero-to (length dfa)))))
    (define (renumber state)
      (cdr (assoc state states)))
    (list->vector
     (map
      (lambda (node)
        (cons (cadr node)
              (map (lambda (x) (cons (car x) (renumber (cdr x))))
                   (cddr node)))) 
      dfa))))

;; Extract all distinct characters or ranges and the potential states
;; they can transition to from a given set of states.  Any ranges that
;; would overlap with distinct characters are split accordingly.
(define (nfa-state-transitions nfa states)
  (let lp ((trans '())   ;; list of (char . state) or ((char . char) . state)
           (ls states)   ;; list of integers (remaining state numbers)
           (res '()))    ;; (char state ...) or ((char . char) state ...)
    (cond
     ((null? trans)
      (if (null? ls)
          (map (lambda (x) (cons (car x) (nfa-closure nfa (cdr x))))
               res)
          (let ((node (assv (car ls) nfa)))
            (lp (if node (cdr node) '()) (cdr ls) res))))
     ((eq? 'epsilon (caar trans))
      (lp (cdr trans) ls res))
     (else
      (lp (cdr trans) ls (nfa-join-transitions! res (car trans)))))))

(define (nfa-join-transitions! existing new)
  (define (join ls elt state)
    (if (not elt)
        ls
        (nfa-join-transitions! ls (cons elt state))))
  (cond
   ((char? (car new))
    (let ((ch (car new)))
      (let lp ((ls existing) (res '()))
        (cond
         ((null? ls)
          ;; done, just cons this on to the original list
          (cons (list ch (cdr new)) existing))
         ((eqv? ch (caar ls))
          ;; add a new state to an existing char
          (set-cdr! (car ls) (insert-sorted (cdr new) (cdar ls)))
          existing)
         ((and (pair? (caar ls))
               (char<=? (caaar ls) ch)
               (char<=? ch (cdaar ls)))
          ;; split a range
          (apply
           (lambda (left right)
             (cons (cons ch (insert-sorted (cdr new) (cdar ls)))
                   (append (if left (list (cons left (cdar ls))) '())
                           (if right (list (cons right (cdar ls))) '())
                           res
                           (cdr ls))))
           (split-char-range (caar ls) (car new))))
         (else
          ;; keep looking
          (lp (cdr ls) (cons (car ls) res)))))))
   (else
    (let ((lo (caar new))
          (hi (cdar new)))
      (let lp ((ls existing) (res '()))
        (cond
         ((null? ls)
          (cons (list (car new) (cdr new)) existing))
         ((and (char? (caar ls)) (char<=? lo (caar ls)) (char<=? (caar ls) hi))
          ;; range enclosing a character
          (apply
           (lambda (left right)
             (set-cdr! (car ls) (insert-sorted (cdr new) (cdar ls)))
             (join (join existing left (cdr new)) right (cdr new)))
           (split-char-range (car new) (caar ls))))
         ((and (pair? (caar ls))
               (or (and (char<=? (caaar ls) hi) (char<=? lo (cdaar ls)))
                   (and (char<=? hi (caaar ls)) (char<=? (cdaar ls) lo))))
          ;; overlapping ranges
          (apply
           (lambda (left1 left2 same right1 right2)
             (let ((old-states (cdar ls)))
               (set-car! (car ls) same)
               (set-cdr! (car ls) (insert-sorted (cdr new) old-states))
               (let* ((res (if right1
                               (cons (cons right1 old-states) existing)
                               existing))
                      (res (if right2 (cons (cons right2 old-states) res) res)))
                 (join (join res left1 (cdr new)) left2 (cdr new)))))
           (intersect-char-ranges (car new) (caar ls))))
         (else
          (lp (cdr ls) (cons (car ls) res)))))))))

(define (char-range c1 c2)
  (if (eqv? c1 c2) c1 (cons c1 c2)))

;; assumes ch is included in the range
(define (split-char-range range ch)
  (list
   (and (not (eqv? ch (car range)))
        (char-range (car range) (integer->char (- (char->integer ch) 1))))
   (and (not (eqv? ch (cdr range)))
        (char-range (integer->char (+ (char->integer ch) 1)) (cdr range)))))

;; returns (possibly #f) char ranges:
;;    a-only-1  a-only-2  a-and-b  b-only-1  b-only-2
(define (intersect-char-ranges a b)
  (if (char>? (car a) (car b))
      (reverse (intersect-char-ranges b a))
      (let ((a-lo (car a))
            (a-hi (cdr a))
            (b-lo (car b))
            (b-hi (cdr b)))
        (list
         (and (char<? a-lo b-lo)
              (char-range a-lo (integer->char (- (char->integer b-lo) 1))))
         (and (char>? a-hi b-hi)
              (char-range (integer->char (+ (char->integer b-hi) 1)) a-hi))
         (char-range b-lo (if (char<? b-hi a-hi) b-hi a-hi))
         #f
         (and (char>? b-hi a-hi)
              (char-range (integer->char (+ (char->integer a-hi) 1)) b-hi))))))

;; The `closure' of a list of NFA states - all states that can be
;; reached from any of them using any number of epsilon transitions.
(define (nfa-closure nfa states)
  (let lp ((ls states)
           (res '()))
    (cond
     ((null? ls)
      res)
     ((memv (car ls) res)
      (lp (cdr ls) res))
     (else
      (lp (append (map cdr
                       (filter (lambda (trans) (eq? 'epsilon (car trans)))
                               (cdr (assv (car ls) nfa))))
                  (cdr ls))
          (insert-sorted (car ls) res))))))

;; insert an integer uniquely into a sorted list
(define (insert-sorted n ls)
  (cond
   ((null? ls)
    (cons n '()))
   ((<= n (car ls))
    (if (= n (car ls))
        ls
        (cons n ls)))
   (else
    (cons (car ls) (insert-sorted n (cdr ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DFAs don't give us match information, so once we match and
;; determine the start and end, we need to recursively break the
;; problem into smaller DFAs to get each submatch.
;;
;; See http://compilers.iecc.com/comparch/article/07-10-026

(define/die (sre-match-extractor sre)
  (let lp ((sre sre) (n 1) (submatch-deps? #f))
    (cond
     ((not (sre-has-submatchs? sre))
      (if (not submatch-deps?)
          (lambda (str i j matches) j)
          (let ((dfa (nfa->dfa (sre->nfa sre))))
            (lambda (str i j matches)
              (dfa-match/longest dfa str i j)))))
     ((pair? sre)
      (case (car sre)
        ((seq)
         (let* ((right (sre-sequence (cddr sre)))
                (match-left (lp (cadr sre) n #t))
                (match-right
                 (lp right (+ n (sre-count-submatchs (cadr sre))) #t)))
           (lambda (str i j matches)
             (let lp ((k j))
               (and (>= k i)
                    (let* ((middle (match-left str i k matches))
                           (end (and middle
                                     (match-right str middle j matches))))
                      (if (eqv? end j)
                          end
                          (lp (- k 1)))))))))
        ((or)
         (let* ((rest (sre-alternate (cddr sre)))
                (match-first
                 (lp (cadr sre) n #t))
                (match-rest
                 (lp rest
                     (+ n (sre-count-submatchs (cadr sre)))
                     submatch-deps?)))
           (lambda (str i j matches)
             (let ((k (match-first str i j matches)))
               (if (eqv? k j)
                   k
                   (match-rest str i j matches))))))
        ((* +)
         (letrec ((match-once
                   (lp (sre-sequence (cdr sre)) n #t))
                  (match-all
                   (lambda (str i j matches)
                     (let ((k (match-once str i j matches)))
                       (if k
                           (match-all str k j matches)
                           i)))))
           (if (eq? '* (car sre))
               match-all
               (lambda (str i j matches)
                 (let ((k (match-once str i j matches)))
                   (and k
                        (match-all str k j matches)))))))
        ((?)
         (let ((match-once (lp (sre-sequence (cdr sre)) n #t)))
           (lambda (str i j matches)
             (let ((k (match-once str i j matches)))
               (if (eqv? k j) k i)))))
        ((submatch)
         (let ((match-one
                (lp (sre-sequence (cdr sre)) (+ n 1) #t)))
           (lambda (str i j matches)
             (let ((res (match-one str i j matches)))
               (and (number? res)
                    (%irregex-match-start-set! matches n i)
                    (%irregex-match-end-set! matches n res)
                    res)))))
        (else
         (die "unknown regexp operator" (car sre)))))
     (else
      (die "unknown regexp" sre)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closure compilation - we use this for non-regular expressions
;; instead of an interpreted NFA matcher

(define/die (sre->procedure sre)
  (let lp ((sre sre) (n 1) (next (lambda (str i matches fail) i)))
    (cond
     ((pair? sre)
      (if (string? (car sre))
          (sre-cset->procedure (sre->cset (car sre)) next)
          (case (car sre)
            ((~ - & /)
             (sre-cset->procedure (sre->cset sre) next))
            ((or)
             (case (length (cdr sre))
               ((0) (lambda (str i matches fail) (fail)))
               ((1) (lp (cadr sre) n next))
               (else
                (let* ((first (lp (cadr sre) n next))
                       (rest (lp (sre-alternate (cddr sre))
                                 (+ n (sre-count-submatchs (cadr sre)))
                                 next)))
                  (lambda (str i matches fail)
                    (first str i matches (lambda () (rest str i matches fail))))))))
            ((seq :)
             (case (length (cdr sre))
               ((0) next)
               ((1) (lp (cadr sre) n next))
               (else
                (let ((rest (lp (sre-sequence (cddr sre))
                                (+ n (sre-count-submatchs (cadr sre)))
                                next)))
                  (lp (cadr sre) n rest)))))
            ((?)
             (let ((body (lp (sre-sequence (cdr sre)) n next)))
               (lambda (str i matches fail)
                 (body str i matches (lambda () (next str i matches fail))))))
            ((??)
             (let ((body (lp (sre-sequence (cdr sre)) n next)))
               (lambda (str i matches fail)
                 (next str i matches (lambda () (body str i matches fail))))))
            ((*)
             (if (sre-empty? (sre-sequence (cdr sre)))
                 (die "invalid sre: empty *" sre)
                 (letrec ((body
                           (lp (sre-sequence (cdr sre))
                               n
                               (lambda (str i matches fail)
                                 (body str
                                       i
                                       matches
                                       (lambda () (next str i matches fail)))))))
                   (lambda (str i matches fail)
                     (body str i matches (lambda () (next str i matches fail)))))))
            ((*?)
             (if (sre-empty? (sre-sequence (cdr sre)))
                 (die "invalid sre: empty *?" sre)
                 (letrec ((body
                           (lp (sre-sequence (cdr sre))
                               n
                               (lambda (str i matches fail)
                                 (next str
                                       i
                                       matches
                                       (lambda () (body str i matches fail)))))))
                   (lambda (str i matches fail)
                     (next str i matches (lambda () (body str i matches fail)))))))
            ((+)
             (lp (sre-sequence (cdr sre))
                 n
                 (lp (list '* (sre-sequence (cdr sre))) n next)))
            ((=)
             (lp `(** ,(cadr sre) ,(cadr sre) ,@(cddr sre)) n next))
            ((>=)
             (lp `(** ,(cadr sre) #f ,@(cddr sre)) n next))
            ((** **?)
             (cond
              ((or (and (number? (cadr sre))
                        (number? (caddr sre))
                        (> (cadr sre) (caddr sre)))
                   (and (not (cadr sre)) (caddr sre)))
               (lambda (str i matches fail) (fail)))
              (else
               (let* ((from (cadr sre))
                      (to (caddr sre))
                      (? (if (eq? '** (car sre)) '? '??))
                      (* (if (eq? '** (car sre)) '* '*?))
                      (sre (sre-sequence (cdddr sre)))
                      (x-sre (sre-strip-submatches sre))
                      (next (if to
                                (if (= from to)
                                    next
                                    (fold (lambda (x next) (lp `(,? ,sre) n next))
                                          next
                                          (zero-to (- to from))))
                                (lp `(,* ,sre) n next))))
                 (if (zero? from)
                     next
                     (lp `(seq ,@(map (lambda (x) x-sre) (zero-to (- from 1)))
                               ,sre)
                         n
                         next))))))
            ((word)
             (lp `(seq bow ,@(cdr sre) eow) n next))
            ((word+)
             (lp `(seq bow (+ (& (or alphanumeric "_")
                                 (or ,@(cdr sre)))) eow)
                 n
                 next))
            ((posix-string)
             (lp (string->sre (cadr sre)) n next))
            ((backref)
             (let ((n (cadr sre)))
               (lambda (str i matches fail)
                (let ((s (irregex-match-substring matches str n)))
                  (if (not s)
                      (fail)
                      (let ((j (+ i (string-length s))))
                        (if (and (<= j (string-length str))
                                 (string=? s (substring str i j)))
                            (next str j matches fail)
                            (fail))))))))
            ((dsm)
             (lp (sre-sequence (cdddr sre)) (+ n (cadr sre)) next))
            ((submatch)
             (let ((body (lp (sre-sequence (cdr sre))
                             (+ n 1)
                             (lambda (str i matches fail)
                               (let ((old (%irregex-match-end matches n)))
                                 (%irregex-match-end-set! matches n i)
                                 (next str i matches
                                       (lambda ()
                                         (%irregex-match-end-set! matches n old)
                                         (fail))))))))
               (lambda (str i matches fail)
                 (let ((old (%irregex-match-start matches n)))
                   (%irregex-match-start-set! matches n i)
                   (body str i matches
                         (lambda ()
                           (%irregex-match-start-set! matches n old)
                           (fail)))))))
            (else
             (die "unknown regexp operator" sre)))))
     ((symbol? sre)
      (case sre
        ((any)
         (lambda (str i matches fail)
           (if (< i (string-length str))
               (next str (+ i 1) matches fail)
               (fail))))
        ((nonl)
         (lambda (str i matches fail)
           (if (and (< i (string-length str))
                    (not (eqv? #\newline (string-ref str i))))
               (next str (+ i 1) matches fail)
               (fail))))
        ((bos)
         (lambda (str i matches fail)
           (if (zero? i) (next str i matches fail) (fail))))
        ((bol)
         (lambda (str i matches fail)
           (if (or (zero? i) (eqv? #\newline (string-ref str (- i 1))))
               (next str i matches fail)
               (fail))))
        ((bow)
         (lambda (str i matches fail)
           (if (and (or (zero? i)
                        (not (char-alphanumeric? (string-ref str (- i 1)))))
                    (< i (string-length str))
                    (char-alphanumeric? (string-ref str i)))
               (next str i matches fail)
               (fail))))
        ((eos)
         (lambda (str i matches fail)
           (if (>= i (string-length str)) (next str i matches fail) (fail))))
        ((eol)
         (lambda (str i matches fail)
           (if (or (>= i (string-length str))
                   (eqv? #\newline (string-ref str i)))
               (next str i matches fail)
               (fail))))
        ((eow)
         (lambda (str i matches fail)
           (if (and (or (>= i (string-length str))
                        (not (char-alphanumeric? (string-ref str i))))
                    (> i 0)
                    (char-alphanumeric? (string-ref str (- i 1))))
               (next str i matches fail)
               (fail))))
        ((word)
         (lp '(seq bow (+ (or alphanumeric #\_)) eow) n next))
        ((epsilon) ;; shouldn't happen
         next)
        (else
         (let ((cell (assq sre sre-named-definitions)))
           (if cell
               (lp (cdr cell) n next)
               (die "unknown regexp" sre))))))
     ((char? sre)
      (lambda (str i matches fail)
        (if (and (< i (string-length str))
                 (eqv? sre (string-ref str i)))
            (next str (+ i 1) matches fail)
            (fail))))
     ((string? sre)
      (lp (sre-sequence (string->list sre)) n next))
     (else
      (die "unknown regexp" sre)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple character sets as lists of ranges, as used in the NFA/DFA
;; compilation.  This is not especially efficient, but is portable and
;; scalable for any range of character sets.

(define (sre-cset->procedure cset next)
  (lambda (str i matches fail)
    (if (and (< i (string-length str))
             (cset-contains? cset (string-ref str i)))
        (next str (+ i 1) matches fail)
        (fail))))

(define (plist->alist ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cddr ls) (cons (cons (car ls) (cadr ls)) res)))))

(define (alist->plist ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cdr ls) (cons (cdar ls) (cons (caar ls) res))))))

(define/die (sre->cset sre . o)
  (let lp ((sre sre) (ci? (and (pair? o) (car o))))
    (define (rec sre) (lp sre ci?))
    (cond
     ((pair? sre)
      (if (string? (car sre))
          (if ci?
              (cset-case-insensitive (string->list (car sre)))
              (string->list (car sre)))
          (case (car sre)
            ((~)
             (cset-complement
              (fold cset-union (rec (cadr sre)) (map rec (cddr sre)))))
            ((&)
             (fold cset-intersection (rec (cadr sre)) (map rec (cddr sre))))
            ((-)
             (fold (lambda (x res) (cset-difference res x))
                   (rec (cadr sre))
                   (map rec (cddr sre))))
            ((/)
             (let ((res (plist->alist (sre-flatten-ranges (cdr sre)))))
               (if ci?
                   (cset-case-insensitive res)
                   res)))
            ((or)
             (fold cset-union (rec (cadr sre)) (map rec (cddr sre))))
            ((w/case)
             (lp (sre-alternate (cdr sre)) #f))
            ((w/nocase)
             (lp (sre-alternate (cdr sre)) #t))
            (else
             (die "not a valid sre char-set operator" sre)))))
     ((char? sre) (rec (list (string sre))))
     ((string? sre) (rec (list sre)))
     (else
      (let ((cell (assq sre sre-named-definitions)))
        (if cell
            (rec (cdr cell))
            (die "not a valid sre char-set" sre)))))))

(define (cset->sre cset)
  (let lp ((ls cset) (chars '()) (ranges '()))
    (cond
     ((null? ls)
      (sre-alternate
       (append
        (if (pair? chars) (list (list (list->string chars))) '())
        (if (pair? ranges) (list (cons '/ (alist->plist ranges))) '()))))
     ((char? (car ls)) (lp (cdr ls) (cons (car ls) chars) ranges))
     (else (lp (cdr ls) chars (cons (car ls) ranges))))))

(define (cset-contains? cset ch)
  (find (lambda (x)
          (or (eqv? x ch)
              (and (pair? x) (char<=? (car x) ch) (char<=? ch (cdr x)))))
        cset))

(define (cset-range x)
  (if (char? x) (cons x x) x))

(define (char-ranges-overlap? a b)
  (if (pair? a)
      (if (pair? b)
          (or (and (char<=? (car a) (cdr b)) (char<=? (car b) (cdr a)))
              (and (char<=? (cdr b) (car a)) (char<=? (cdr a) (car b))))
          (and (char<=? (car a) b) (char<=? b (cdr a))))
      (if (pair? b)
          (char-ranges-overlap? b a)
          (eqv? a b))))

(define (char-ranges-union a b)
  (cons (if (char<=? (car a) (car b)) (car a) (car b))
        (if (char>=? (cdr a) (cdr b)) (cdr a) (cdr b))))

(define (cset-union a b)
  (cond ((null? b) a)
        ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
         => (lambda (ls)
              (cset-union
               (cset-union (append (take-up-to a ls) (cdr ls))
                           (list (char-ranges-union (cset-range (car ls))
                                                    (cset-range (car b)))))
               (cdr b))))
        (else (cset-union (cons (car b) a) (cdr b)))))

(define (cset-difference a b)
  (cond ((null? b) a)
        ((not (car b)) (cset-difference a (cdr b)))
        ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
         => (lambda (ls)
              (apply
               (lambda (left1 left2 same right1 right2)
                 (let* ((a (append (take-up-to a ls) (cdr ls)))
                        (a (if left1 (cons left1 a) a))
                        (a (if left2 (cons left2 a) a))
                        (b (if right1 (cset-union b (list right1)) b))
                        (b (if right2 (cset-union b (list right2)) b)))
                   (cset-difference a b)))
               (intersect-char-ranges (cset-range (car ls))
                                      (cset-range (car b))))))
        (else (cset-difference a (cdr b)))))

(define (cset-intersection a b)
  (let intersect ((a a) (b b) (res '()))
    (cond ((null? b) res)
          ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
           => (lambda (ls)
                (apply
                 (lambda (left1 left2 same right1 right2)
                   (let* ((a (append (take-up-to a ls) (cdr ls)))
                          (a (if left1 (cons left1 a) a))
                          (a (if left2 (cons left2 a) a))
                          (b (if right1 (cset-union b (list right1)) b))
                          (b (if right2 (cset-union b (list right2)) b)))
                     (intersect a b (cset-union res (list same)))))
                 (intersect-char-ranges (cset-range (car ls))
                                        (cset-range (car b))))))
          (else (intersect a (cdr b) res)))))

(define (cset-complement a)
  (cset-difference (list (cons *min-char* *max-char*)) a))

(define (cset-case-insensitive a)
  (let lp ((ls a) (res '()))
    (cond ((null? ls) (reverse res))
          ((and (char? (car ls)) (char-alphabetic? (car ls)))
           (let ((c2 (char-altcase (car ls)))
                 (res (cons (car ls) res)))
             (lp (cdr ls) (if (cset-contains? res c2) res (cons c2 res)))))
          ((and (pair? (car ls))
                (char-alphabetic? (caar ls))
                (char-alphabetic? (cdar ls)))
           (lp (cdr ls)
               (cset-union (cset-union res (list (car ls)))
                           (list (cons (char-altcase (caar ls))
                                       (char-altcase (cdar ls)))))))
          (else (lp (cdr ls) (cset-union res (list (car ls))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match and replace utilities

(define (irregex-replace irx str . o)
  (let ((m (irregex-search (irregex irx) str)))
    (and
     m
     (string-cat-reverse
      (cons (substring str (irregex-match-end m str 0) (string-length str))
            (append (irregex-apply-match m str o)
                    (list (substring str 0 (irregex-match-start m str 0)))))))))

(define (irregex-replace/all irx str . o)
  (let* ((irx (irregex irx))
         (matches (irregex-make-matches irx))
         (end (string-length str)))
    (let lp ((i 0) (res '()))
      (let ((m (irregex-search/matches irx str i end matches)))
        (if (not m)
            (string-cat-reverse
             (cons (substring str i (string-length str)) res))
            (let* ((start (irregex-match-start m str 0))
                   (end (irregex-match-end m str 0))
                   (res (append (irregex-apply-match m str o)
                                (cons (substring str i start) res))))
              (irregex-reset-matches! matches)
              (lp end res)))))))

(define/die (irregex-apply-match m str ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        res
        (cond
         ((integer? (car ls))
          (lp (cdr ls)
              (cons (or (irregex-match-substring m str (car ls)) "") res)))
         ((procedure? (car ls))
          (lp (cdr ls) (cons ((car ls) m str) res)))
         ((symbol? (car ls))
          (case (car ls)
            ((pre)
             (lp (cdr ls)
                 (cons (substring str 0 (irregex-match-start m str 0)) res)))
            ((post)
             (lp (cdr ls)
                 (cons (substring str
                                  (irregex-match-end m str 0)
                                  (string-length str))
                       res)))
            (else (die "unknown match replacement" (car ls)))))
         (else
          (lp (cdr ls) (cons (car ls) res)))))))

;;;; irregex-utils.scm
;;
;; Copyright (c) 2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define rx-special-chars
  "\\|[](){}.*+?^$")

(define (irregex-quote str)
  (list->string
   (let loop ((ls (string->list str)) (res '()))
     (if (null? ls)
       (reverse res)
       (let ((c (car ls)))
         (if (string-scan-char rx-special-chars c)
           (loop (cdr ls) (cons c (cons #\\ res)))
           (loop (cdr ls) (cons c res))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (irregex-opt ls)
  (cond
    ((null? ls) "")
    ((null? (cdr ls)) (irregex-quote (car ls)))
    (else
     (let ((chars (make-vector 256 '())))
       (let lp1 ((ls ls) (empty? #f))
         (if (null? ls)
           (string-append
            "(?:"
            (string-intersperse
             (let lp2 ((i 0) (res '()))
               (if (= i 256)
                 (reverse res)
                 (let ((c (integer->char i))
                       (opts (vector-ref chars i)))
                   (lp2 (+ i 1)
                        (if (null? opts)
                          res
                          (cons (string-append
                                 (irregex-quote (string c))
                                 (irregex-opt opts))
                                res))))))
             "|")
            (if empty? "|" "") ; or use trailing '?' ?
            ")")
           (let* ((str (car ls))
                  (len (string-length str)))
             (if (zero? len)
               (lp1 (cdr ls) #t)
               (let ((i (char->integer (string-ref str 0))))
                 (vector-set!
                  chars
                  i
                  (cons (substring str 1 (string-length str))
                        (vector-ref chars i)))
                 (lp1 (cdr ls) empty?))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cset->string ls)
  (call-with-string-output-port
    (lambda (sop)
      (define (d x) (display x sop))
      (let lp ((ls ls))
        (unless (null? ls)
          (cond
            ((pair? (car ls))
             (d (irregex-quote (string (caar ls))))
             (d "-")
             (d (irregex-quote (string (cdar ls)))))
            (else (d (irregex-quote (string (car ls))))))
          (lp (cdr ls)))))))

(define/die (sre->string obj)
  (call-with-string-output-port
    (lambda (sop)
      (define (d x) (display x sop))
      (let lp ((x obj))
        (cond
          ((pair? x)
           (case (car x)
             ((: seq)
              (cond
                ((and (pair? (cddr x)) (pair? (cddr x)) (not (eq? x obj)))
                 (d "(?:") (for-each lp (cdr x)) (d ")"))
                (else (for-each lp (cdr x)))))
             ((submatch) (d "(") (for-each lp (cdr x)) (d ")"))
             ((\x7C; or)  ;; \x7C; is (string->symbol "|")
              (d "(?:")
              (lp (cadr x))
              (for-each (lambda (x) (d "|") (lp x)) (cddr x))
              (d ")"))
             ((* + ?)
              (cond
                ((pair? (cddr x))
                 (d "(?:") (for-each lp (cdr x)) (d ")"))
                (else (lp (cadr x))))
              (d (car x)))
             ((not)
              (cond
                ((and (pair? (cadr x)) (eq? 'cset (caadr x)))
                 (d "[^")
                 (d (cset->string (cdadr x)))
                 (d "]"))
                (else (die "can't represent general 'not' in strings" x))))
             ((cset)
              (d "[")
              (d (cset->string (cdr x)))
              (d "]"))
             ((w/case w/nocase)
              (d "(?")
              (if (eq? (car x) 'w/case) (d "-"))
              (d ":")
              (for-each lp (cdr x))
              (d ")"))
             (else (die "unknown match operator" x))))
          ((symbol? x)
           (case x
             ((bos bol) (d "^"))
             ((eos eol) (d "$"))
             ((any nonl) (d "."))
             (else (die "unknown match symbol" x))))
          ((string? x)
           (d (irregex-quote x)))
          (else (die "unknown match pattern" x)))))))
)
