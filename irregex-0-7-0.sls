#!r6rs
(library (xitomatl irregex (0 7 0))
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
    irregex-nfa irregex-flags irregex-submatches irregex-lengths irregex-names
    irregex-quote irregex-opt sre->string string->sre
    ;; Xitomatl additions
    irregex-search/all irregex-search/all/strings
    irregex-search/chunked/all irregex-search/chunked/all/strings
    make-pair-chain-chunker pair-chain-chunking-lose-refs!
    list-chunker list-chunking-lose-refs!
    port-chunker make-port-chunker port-chunking-lose-refs!
    #;irregex-search-port irregex-search-port/all irregex-search-port/all/strings)
  (import
    (except (rnrs) error remove)
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (xitomatl include)
    (only (xitomatl define extras) define/? define/?/AV)
    (only (xitomatl strings) string-intersperse)
    (only (xitomatl common-unstandard) with-output-to-string)
    (only (xitomatl srfi strings) string-concatenate-reverse)
    (only (xitomatl predicates) exact-non-negative-integer? exact-positive-integer?)
    (only (xitomatl ports) textual-input-port?))

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
  
  (define/?/AV irregex-search/all
    (case-lambda/?
      [(irx str) 
       (irregex-search/all irx str 0)]
      [(irx str start)
       (irregex-search/all irx str start (string-length str))]
      [(irx str start end)
       (irregex-search/all irx str start end values)]
      [(irx [str string?] 
        [start exact-non-negative-integer?] [end exact-non-negative-integer?]
        [proc procedure?])
       (let ([irx-c (irregex irx)])
         (let loop ([i start] 
                    [accum '()])
           (let ([m (irregex-search irx-c str i end)])
             (if m
               (let ([end-index (irregex-match-end-index m 0)])
                 (when (>= i end-index)  ;; Prevents infinite loops for some regexs.
                   (AV "pattern not advancing search" irx start i end-index end))
                 (loop end-index
                       (cons (proc m) accum)))
               (reverse accum)))))]))
  
  (define irregex-search/all/strings
    (case-lambda
      [(irx str) 
       (irregex-search/all/strings irx str 0)]
      [(irx str start)
       (irregex-search/all/strings irx str start (string-length str))]
      [(irx str start end)
       (irregex-search/all irx str start end irregex-match-substring)]))
  
  ;;--------------------------------------------------------------------------
  
  (define (irregex-chunker? x)
    (and (vector? x)
         (= 6 (vector-length x))
         (procedure? (chunk-get-next x))
         (procedure? (chunk-get-str x))
         (procedure? (chunk-get-start x))
         (procedure? (chunk-get-end x))
         (procedure? (chunk-get-substring x))
         (or (procedure? (chunk-get-subchunk x))
             (not (chunk-get-subchunk x)))))
  
  (define/?/AV irregex-search/chunked/all
    (case-lambda/?
      [(irx chunker chunk)
       (irregex-search/chunked/all irx chunker chunk #f)]
      [(irx chunker chunk lose-refs!)
       (irregex-search/chunked/all irx chunker chunk lose-refs! values)]
      [(irx [chunker irregex-chunker?] chunk 
        [lose-refs! (lambda (x) (or (not x) (procedure? x)))] 
        [proc procedure?])
       (let ([irx-c (irregex irx)]
             [get-start (chunk-get-start chunker)]
             [get-end (chunk-get-end chunker)]
             [get-subchunk (chunk-get-subchunk chunker)])
         ;; This must be true of the get-subchunk of the chunker used with this procedure: 
         ;; (eq? (get-next end) (get-next (get-subchunk start i end j))) 
         (unless (procedure? get-subchunk)
           (AV "get-subchunk not a procedure" get-subchunk))
         (let loop ([chk chunk] 
                    [accum '()])
           (let ([m (irregex-search/chunked irx-c chunker chk)])
             (if m
               (let ([end-chunk (irregex-match-end-source m 0)]
                     [end-index (irregex-match-end-index m 0)])
                 (when (and (eq? chk end-chunk)
                            (>= (get-start chk) end-index))
                   (AV "pattern not advancing search" irx (get-start chk) end-index))
                 (let ([resume-chunk (get-subchunk end-chunk end-index
                                                   end-chunk (get-end end-chunk))])
                   (when lose-refs!
                     ;; Losing possible reference(s) reachable from the match 
                     ;; object to chunk(s) outside the match chunks is done to
                     ;; allow outside chunk(s) to be GC'ed when they're no longer
                     ;; needed during the search, which is necessary for efficient
                     ;; memory usage.
                     ;; lose-refs! must return chunks that will work with the rest
                     ;; of chunker's procedures.
                     (let ([replacements
                            (let loop ([n (irregex-match-num-submatches m)]
                                       [submatch-chunks '()])
                              (if (negative? n)
                                (lose-refs! submatch-chunks)
                                (loop (- n 1) 
                                      (cons* (irregex-match-start-source m n)
                                             (irregex-match-end-source m n)
                                             submatch-chunks))))])
                       (unless (and (list? replacements)
                                    (let ([l (length replacements)])
                                      (and (>= l 2) (even? l))))
                         (AV "lose-refs! returned invalid value" replacements))
                       (let loop ([r replacements] [n 0])
                         (unless (null? r)
                           (irregex-match-start-source-set! m n (car r))
                           (irregex-match-end-source-set! m n (cadr r))
                           (loop (cddr r) (+ 1 n))))))
                   (loop resume-chunk
                         (cons (proc m) accum))))
               (reverse accum)))))]))
  
  (define (irregex-search/chunked/all/strings irx chunker chunk)
    ;; NOTE: Don't need to supply a lose-refs! because the match objects
    ;;       are immediately lost after given to irregex-match-substring.
    (irregex-search/chunked/all irx chunker chunk #f irregex-match-substring))
  
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
                  (string-concatenate-reverse (cons (substring (car next) 0 end) res))
                  (loop (cdr next) 
                        (cons (car next) res))))))]
         [get-subchunk 
          (lambda (chunkA start chunkB end)
            (cons (get-substring chunkA start chunkB end) 
                  (cdr chunkB)))])
      (make-irregex-chunker get-next get-string get-start get-end
                            get-substring get-subchunk)))
  
  (define (pair-chain-chunking-lose-refs! submatch-chunks)
    ;; submatch-chunks ::= (<submatch-0-start-chunk> <submatch-0-end-chunk>
    ;;                      <submatch-1-start-chunk> <submatch-1-end-chunk>
    ;;                      ...                      ...)
    (let ([last-chunk (cadr submatch-chunks)])      
      (set-cdr! last-chunk '())  ;; lose the reference to chunks outside this match
      (assert (let ([lost-ref?
                     (lambda (chunk)
                       (let loop ([c chunk] [found-last #f])
                         (if (null? c)
                           (and found-last (null? (cdr found-last)))
                           (begin
                             (assert (pair? c))  ;; the next chunk must already exist
                             (if (eq? c last-chunk)
                               (loop '() c)
                               (loop (cdr c) #f))))))])
                (for-all lost-ref? submatch-chunks)))
      submatch-chunks))
  
  ;;--------------------------------------------------------------------------
  
  (define list-chunker
    (make-pair-chain-chunker 
     (letrec ([get-next (lambda (chunk)
                          (let ([r (cdr chunk)])
                            (and (pair? r)
                                 (if (string=? "" (car r))
                                   (get-next r)
                                   r))))])
       get-next)))
  
  (define list-chunking-lose-refs! pair-chain-chunking-lose-refs!)
  
  ;;--------------------------------------------------------------------------
  
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
  
  (define port-chunking-lose-refs! pair-chain-chunking-lose-refs!)
  
  (define port-chunker (make-port-chunker 128))  ;; good default size?
  
  #;(define/? irregex-search-port 
    ;; NOTE: Users of this procedure need to be aware that the match object
    ;;       returned references chunk object(s) which might reference the port.
    ;;       Users should lose the match object to lose the reference to the port.
    (let ([port-chunker-one (make-port-chunker 1)])
      ;; port-chunker-one is necessary so that get-next will not read passed
      ;; the match, which would advance the port too far.  This is probably
      ;; horribly slow, and this procedure might be deleted.
      (lambda/? (irx [port textual-input-port?])
        (irregex-search/chunked irx port-chunker-one (cons "" port)))))
    
  (define/? irregex-search-port/all
    (case-lambda/?
      [(irx port)
       (irregex-search-port/all irx port values)]
      [(irx port proc)
       (irregex-search-port/all irx port proc port-chunker)]
      [(irx [port textual-input-port?] proc chunker)
       ;; chunker must be a chunker returned by make-port-chunker
       (irregex-search/chunked/all irx chunker (cons "" port)
                                   ;; TODO: test memory consumption is efficient
                                   port-chunking-lose-refs! proc)]))
  
  (define/? irregex-search-port/all/strings
    ;; TODO: test memory consumption is efficient
    (case-lambda/?
      [(irx port)
       (irregex-search-port/all/strings irx port port-chunker)]
      [(irx [port textual-input-port?] chunker)
       ;; chunker must be a chunker returned by make-port-chunker
       (irregex-search/chunked/all/strings irx chunker (cons "" port))]))
  
  ;;--------------------------------------------------------------------------
  
  ;; TODO? irregex-search-enumerator for use with (xitomatl enumerators) 
  ;;       which is as lazy and memory efficient as possible
  ;;       and cleans-up on early termination.
)
