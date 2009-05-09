;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (xitomatl irregex extras)
  (export
    irregex-search/all irregex-search/all/strings
    irregex-search/chunked/all irregex-search/chunked/all/strings
    irregex-search-port/all irregex-search-port/all/strings
    irregex-chunk-enumerator irregex-string-enumerator
    irregex-list-enumerator irregex-port-enumerator irregex-enumerator
    list-chunker list-chunking-lose-refs
    port-chunker make-port-chunker port-chunking-lose-refs
    port-chunking-make-initial-chunk make-lose-refs)
  (import
    (rename (rnrs) (assert rnrs:assert))
    (only (xitomatl define) define/? define/AV)
    (only (xitomatl predicates) exact-positive-integer? or?)
    (only (xitomatl ports) textual-input-port?)
    (only (xitomatl enumerators) fold/enumerator)
    (xitomatl irregex (or (0 7 (>= 3))
                          (0 (>= 8))
                          ((>= 1)))))

  (define-syntax assert
    (syntax-rules ()
      [(_ expr) (rnrs:assert expr)]
      #;[(_ expr) #f]))

  (define irregex-search/all
    (case-lambda
      [(irx str)
       (irregex-search/all irx str 0)]
      [(irx str start)
       (irregex-search/all irx str start (string-length str))]
      [(irx str start end)
       (irregex-search/all irx str start end values)]
      [(irx str start end proc)
       (reverse
        (fold/enumerator (irregex-string-enumerator irx start end)
                         str
                         (lambda (m a) (values #t (cons (proc m) a)))
                         '()))]))

  (define irregex-search/all/strings
    (case-lambda
      [(irx str)
       (irregex-search/all/strings irx str 0)]
      [(irx str start)
       (irregex-search/all/strings irx str start (string-length str))]
      [(irx str start end)
       (irregex-search/all irx str start end irregex-match-substring)]))

  (define irregex-search/chunked/all
    (case-lambda
      [(irx chunker chunk)
       (irregex-search/chunked/all irx chunker chunk #f)]
      [(irx chunker chunk lose-refs)
       (irregex-search/chunked/all irx chunker chunk lose-refs values)]
      [(irx chunker chunk lose-refs proc)
       (reverse
        (fold/enumerator (irregex-chunk-enumerator irx chunker lose-refs)
                         chunk
                         (lambda (m a) (values #t (cons (proc m) a)))
                         '()))]))

  (define (irregex-search/chunked/all/strings irx chunker chunk)
    ;; NOTE: Don't need to supply a lose-refs because the match objects
    ;;       are immediately lost after given to irregex-match-substring.
    (irregex-search/chunked/all irx chunker chunk #f irregex-match-substring))

  (define irregex-search-port/all
    (case-lambda
      [(irx port)
       (irregex-search-port/all irx port values)]
      [(irx port proc)
       (irregex-search-port/all irx port proc #f)]
      [(irx port proc chunk-size)
       (irregex-search/chunked/all irx (if chunk-size
                                         (make-port-chunker chunk-size)
                                         port-chunker)
                                   (port-chunking-make-initial-chunk port)
                                   port-chunking-lose-refs proc)]))

  (define irregex-search-port/all/strings
    (case-lambda
      [(irx port)
       (irregex-search-port/all/strings irx port #f)]
      [(irx port chunk-size)
       (irregex-search/chunked/all/strings irx (if chunk-size
                                                 (make-port-chunker chunk-size)
                                                 port-chunker)
                                           (port-chunking-make-initial-chunk port))]))

  ;;--------------------------------------------------------------------------

  (define/AV irregex-chunk-enumerator
    (case-lambda
      [(irx chunker)
       (irregex-chunk-enumerator irx chunker #f)]
      [(irx chunker lose-refs)
       (let ([irx-c (irregex irx)]
             [get-start (chunker-get-start chunker)])
         (lambda (chunk proc seeds)
           (let loop ([chk chunk] [i (get-start chunk)] [seeds seeds])
             (let ([m (irregex-search/chunked irx-c chunker chk i)])
               (if m
                 (let ([end-chunk (irregex-match-end-source m 0)]
                       [end-index (irregex-match-end-index m 0)])
                   (when lose-refs
                     ;; Losing possible reference(s) reachable from the match
                     ;; object to chunk(s) outside the match chunks is done to
                     ;; allow outside chunk(s) to be GC'ed when they're no
                     ;; longer needed during the search, which is necessary for
                     ;; efficient memory usage.  lose-refs must return chunks
                     ;; which will work with chunker's procedures.  lose-refs
                     ;; must not mutate the chunks given to it, it must return
                     ;; new chunk objects referring to the same underlying
                     ;; string pieces but not referring to chunks outside the
                     ;; match range.  The only thing mutated is the new match
                     ;; object which was made just for us.
                     (let ([replacements
                            (let loop ([n (irregex-match-num-submatches m)]
                                       [submatch-chunks '()])
                              (if (negative? n)
                                (lose-refs submatch-chunks)
                                (loop (- n 1)
                                      (cons* (irregex-match-start-source m n)
                                             (irregex-match-end-source m n)
                                             submatch-chunks))))])
                       (assert (and (list? replacements)
                                    (let ([l (length replacements)])
                                      (and (>= l 2) (even? l)))))
                       (let loop ([r replacements] [n 0])
                         (unless (null? r)
                           (irregex-match-start-source-set! m n (car r))
                           (irregex-match-end-source-set! m n (cadr r))
                           (loop (cddr r) (+ 1 n))))))
                   (let-values ([(continue . next-seeds) (apply proc m seeds)])
                     (if continue
                       (if (or (not (eq? chk end-chunk))
                               (< i end-index))
                         (loop end-chunk end-index next-seeds)
                         (AV "pattern not advancing search" irx))
                       (apply values next-seeds))))
                 (apply values seeds))))))]))

  (define irregex-string-enumerator
    (let ((string-chunker
           (make-irregex-chunker (lambda (_) #F) car cadr caddr)))
      (case-lambda
        [(irx)
         (irregex-string-enumerator irx 0)]
        [(irx start)
         (irregex-string-enumerator irx start #f)]
        [(irx start end)
         (let ((ce (irregex-chunk-enumerator irx string-chunker)))
           (lambda (str proc seeds)
             (ce (list str start (or end (string-length str)))
                 proc seeds)))])))

  (define (irregex-list-enumerator irx)
    (let ([ce (irregex-chunk-enumerator irx list-chunker list-chunking-lose-refs)])
      (lambda (l proc seeds)
        (if (null? l)
          (apply values seeds)
          (ce l proc seeds)))))

  (define irregex-port-enumerator
    (case-lambda
      [(irx)
       (irregex-port-enumerator irx #f)]
      [(irx chunk-size)
       (let ([ce (irregex-chunk-enumerator irx (if chunk-size
                                                 (make-port-chunker chunk-size)
                                                 port-chunker)
                                           port-chunking-lose-refs)])
         (lambda (port proc seeds)
           (ce (port-chunking-make-initial-chunk port) proc seeds)))]))

  (define/AV (irregex-enumerator irx)
    (lambda (coll proc seeds)
      (cond
        [(string? coll)
         ((irregex-string-enumerator irx) coll proc seeds)]
        [(list? coll)
         ((irregex-list-enumerator irx) coll proc seeds)]
        [(textual-input-port? coll)
         ((irregex-port-enumerator irx) coll proc seeds)]
        [else
         (AV "invalid collection type" coll)])))

  ;;--------------------------------------------------------------------------

  (define (make-lose-refs chunker make-chunk)
    (let ((get-next (chunker-get-next chunker)))
      (lambda (submatch-chunks)
        ;; submatch-chunks ::= (<submatch-0-start-chunk> <submatch-0-end-chunk>
        ;;                      <submatch-1-start-chunk> <submatch-1-end-chunk>
        ;;                      ...                      ...)
        (let ([first (car submatch-chunks)]
              [last (cadr submatch-chunks)])
          (let* ([reversed-chain
                  (let loop ([chain first] [rev '()])
                    (if (eq? chain last)
                      (cons chain rev)
                      (loop (get-next chain) (cons chain rev))))]
                 [new-first
                  (let loop ([rev reversed-chain] [nc #F])
                    (if (null? rev)
                      nc
                      (loop (cdr rev) (make-chunk (car rev) nc))))]
                 [correlated
                  (let loop ([new-chain new-first] [chain first] [alist '()])
                    (if new-chain
                      (loop (get-next new-chain)
                            (get-next chain)
                            (cons (cons chain new-chain) alist))
                      alist))])
            (map (lambda (c) (and c (cdr (assq c correlated))))
                 submatch-chunks))))))
 
  (define list-chunker
    (make-irregex-chunker
     (letrec ([get-next (lambda (chunk)
                          (let ([r (cdr chunk)])
                            (and (pair? r)
                                 (if (string=? "" (car r))
                                   (get-next r)
                                   r))))])
       get-next)
     car))

  (define list-chunking-lose-refs
    (make-lose-refs list-chunker
                    (lambda (old-chunk new-next-chunk)
                      (cons (car old-chunk)
                            (or new-next-chunk '())))))

  (define make-port-chunk vector)
  (define (port-chunk-str pc) (vector-ref pc 0))
  (define (port-chunk-next pc) (vector-ref pc 1))
  (define (port-chunk-next-set! pc n) (vector-set! pc 1 n))

  (define/? (make-port-chunker [chunk-size exact-positive-integer?])
    (make-irregex-chunker
     (lambda (chunk)
       (let ((n (port-chunk-next chunk)))
         (if (port? n)
           (let* ((s (get-string-n n chunk-size))
                  (next (and (string? s) (make-port-chunk s n))))
             (port-chunk-next-set! chunk next)
             next)
           n)))
     port-chunk-str))

  (define port-chunker (make-port-chunker #x400))  ;; good default size?

  (define port-chunking-lose-refs
    (make-lose-refs port-chunker
                    (lambda (old-chunk new-next-chunk)
                      (make-port-chunk (port-chunk-str old-chunk)
                                       new-next-chunk))))

  (define (port-chunking-make-initial-chunk port)
    (make-port-chunk "" port))
)
