#!r6rs
(library (xitomatl irregex (0 7 1))
  (export
    irregex string->irregex sre->irregex irregex? irregex-match-data?
    irregex-new-matches irregex-reset-matches!
    irregex-match-start-source irregex-match-start-index 
    irregex-match-end-source irregex-match-end-index
    irregex-match-num-submatches irregex-match-substring irregex-match-index
    irregex-search irregex-search/matches irregex-match
    irregex-replace irregex-replace/all irregex-fold
    irregex-search/chunked irregex-match/chunked
    make-irregex-chunker irregex-match-subchunk
    irregex-dfa irregex-dfa/search irregex-dfa/extract
    irregex-nfa irregex-flags irregex-num-submatches irregex-lengths irregex-names
    irregex-quote irregex-opt sre->string string->sre
    ;; Xitomatl additions
    irregex-search/all irregex-search/all/strings
    irregex-search/chunked/all irregex-search/chunked/all/strings
    irregex-search-port/all irregex-search-port/all/strings
    irregex-enumerator irregex-string-enumerator irregex-chunk-enumerator 
    irregex-list-enumerator irregex-port-enumerator
    make-pair-chain-chunker pair-chain-chunking-lose-refs
    list-chunker list-chunking-lose-refs
    port-chunker make-port-chunker port-chunking-lose-refs 
    port-chunking-make-initial-chunk)
  (import
    (except (rnrs) error remove assert)
    (prefix (only (rnrs) assert) rnrs:)
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (only (xitomatl include) include/resolve)
    (only (xitomatl define extras) define/? define/AV)
    (only (xitomatl strings) string-intersperse)
    (only (xitomatl common) with-output-to-string)
    (only (xitomatl predicates) exact-positive-integer?)
    (only (xitomatl ports) textual-input-port?)
    (only (xitomatl enumerators) fold/enumerator))
  
  (define-syntax assert
    (syntax-rules ()
      [(_ expr) (rnrs:assert expr)]
      #;[(_ expr) #f]))
  
  ;; --- Needed for irregex-r6rs.scm and irregex-utils.scm -------------------

  (define (error . args)
    (apply assertion-violation "(library (xitomatl irregex))" args))

  (define-syntax any 
    (syntax-rules ()
      [(_ pred ls) 
       (exists pred ls)]))
  
  (define-syntax every
    (syntax-rules ()
      [(_ pred ls)
       (for-all pred ls)]))
  
  (define-syntax remove
    (syntax-rules ()
      [(_ pred ls)
       (remp pred ls)]))
  
  (define-syntax ->string
    (syntax-rules ()
      [(_ expr)
       expr]))
  
  (include/resolve ("xitomatl" "irregex") "irregex-r6rs.scm")
  (include/resolve ("xitomatl" "irregex") "irregex-utils.scm")
  
  ;; --- Xitomatl additions --------------------------------------------------
  
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
  
  (define/AV irregex-string-enumerator 
    (case-lambda
      [(irx)
       (irregex-string-enumerator irx 0)]
      [(irx start)
       (irregex-string-enumerator irx start #f)]
      [(irx start end)
       (let ([irx-c (irregex irx)])
         (lambda (str proc seeds)
           (let ([end (or end (string-length str))])
             (let loop ([i start] [seeds seeds])
               (let ([m (irregex-search irx-c str i end)])
                 (if m
                   (let ([end-index (irregex-match-end-index m 0)])
                     (unless (< i end-index)  ;; Prevents infinite loops for some regexs.
                       (AV "pattern not advancing search" irx start i end-index end))
                     (let-values ([(continue . next-seeds) (apply proc m seeds)])
                       (if continue
                         (loop end-index next-seeds)
                         (apply values next-seeds))))
                   (apply values seeds)))))))]))
  
  (define/AV irregex-chunk-enumerator 
    (case-lambda
      [(irx chunker)
       (irregex-chunk-enumerator irx chunker #f)]
      [(irx chunker lose-refs)
       (let ([irx-c (irregex irx)]
             [get-start (chunker-get-start chunker)]
             [get-end (chunker-get-end chunker)]
             [get-subchunk (chunker-get-subchunk chunker)])
         (assert (procedure? get-subchunk))
         ;; This must be true of the get-subchunk of the chunker used with this
         ;; enumerator: (eq? (get-next end) (get-next (get-subchunk start i end j))) 
         (lambda (chunk proc seeds)
           (let loop ([chk chunk] [seeds seeds])
             (let ([m (irregex-search/chunked irx-c chunker chk)])
               (if m
                 (let ([end-chunk (irregex-match-end-source m 0)]
                       [end-index (irregex-match-end-index m 0)])
                   (when (and (eq? chk end-chunk)
                              (>= (get-start chk) end-index))
                     (AV "pattern not advancing search" irx (get-start chk) end-index))
                   (when lose-refs
                     ;; Losing possible reference(s) reachable from the match 
                     ;; object to chunk(s) outside the match chunks is done to
                     ;; allow outside chunk(s) to be GC'ed when they're no longer
                     ;; needed during the search, which is necessary for efficient
                     ;; memory usage.  lose-refs must return chunks that will work
                     ;; with chunker's other procedures.  lose-refs must not
                     ;; mutate the chunks given to it.  The only thing mutated is
                     ;; the new match object which was made just for us.
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
                       ;; TODO? Use a better chunk representation which includes start
                       ;; and end indexes, to allow below get-subchunk to be more
                       ;; effecient? Will need converting of lists of strings...
                       (loop (get-subchunk end-chunk end-index 
                                           end-chunk (get-end end-chunk))
                             next-seeds)
                       (apply values next-seeds))))
                 (apply values seeds))))))]))
  
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
  
  ;;--------------------------------------------------------------------------
  
  (define/? (make-pair-chain-chunker [get-next procedure?])
    (letrec 
        ([get-string car]
         [get-start  (lambda (chunk) 0)]
         [get-end    (lambda (chunk) (string-length (car chunk)))]
         [get-substring 
          (lambda (chunkA start chunkB end)
            (if (eq? chunkA chunkB)
              (substring (car chunkA) start end)
              (let loop ([next (cdr chunkA)]
                         [res (list (let ([s (car chunkA)])
                                      (substring s start (string-length s))))])
                (assert (pair? next))
                (if (eq? next chunkB)
                  (string-cat-reverse (cons (substring (car next) 0 end) res))
                  (loop (cdr next) (cons (car next) res))))))]
         [get-subchunk 
          (lambda (chunkA start chunkB end)
            (cons (get-substring chunkA start chunkB end) 
                  (cdr chunkB)))])
      (make-irregex-chunker get-next get-string get-start get-end
                            get-substring get-subchunk)))
  
  (define (pair-chain-chunking-lose-refs submatch-chunks)
    (assert (<= 2 (length submatch-chunks)))
    ;; submatch-chunks ::= (<submatch-0-start-chunk> <submatch-0-end-chunk>
    ;;                      <submatch-1-start-chunk> <submatch-1-end-chunk>
    ;;                      ...                      ...)
    (let ([first (car submatch-chunks)] 
          [last (cadr submatch-chunks)])
      (assert (let loop ([chain first])
                (assert (pair? chain))
                (or (eq? chain last)
                    (loop (cdr chain)))))
      (let* ([reversed-chain 
              (let loop ([chain first] [rev '()])
                (if (eq? chain last)
                  (cons chain rev)
                  (loop (cdr chain) (cons chain rev))))]
             [new-first 
              (let loop ([rev reversed-chain] [nc '()])
                (if (null? rev)
                  nc              ;; this cons makes the new chunks chain
                  (loop (cdr rev) (cons (caar rev) nc))))] 
             [correlated 
              (let loop ([new-chain new-first] [chain first] [alist '()])
                (if (null? new-chain)
                  alist
                  (loop (cdr new-chain) (cdr chain) (cons (cons chain new-chain) 
                                                          alist))))])
        (map (lambda (c)
               (cdr (assq c correlated)))
             submatch-chunks))))
  
  (define list-chunker
    (make-pair-chain-chunker 
     (letrec ([get-next (lambda (chunk)
                          (let ([r (cdr chunk)])
                            (and (pair? r)
                                 (if (string=? "" (car r))
                                   (get-next r)
                                   r))))])
       get-next)))
  
  (define list-chunking-lose-refs pair-chain-chunking-lose-refs)
  
  (define/? (make-port-chunker [chunk-size exact-positive-integer?])
    (make-pair-chain-chunker 
     (lambda (chunk)
       (let ([r (cdr chunk)])
         (cond [(pair? r) r]
               [(port? r)
                (let ([s (get-string-n r chunk-size)])
                  (if (eof-object? s)
                    (begin (set-cdr! chunk '())
                           #f)
                    (let ([next (cons s r)])
                      (assert (positive? (string-length s)))
                      (set-cdr! chunk next)
                      next)))]
               [(null? r) #f])))))
  
  (define port-chunking-lose-refs pair-chain-chunking-lose-refs)
  
  (define (port-chunking-make-initial-chunk port)
    (cons "" port))
  
  (define port-chunker (make-port-chunker 128))  ;; good default size?
  
)
