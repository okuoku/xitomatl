#!r6rs
(library (xitomatl irregex (0 6 1))
  (export
    irregex string->irregex sre->irregex irregex? irregex-match-data?
    irregex-new-matches irregex-reset-matches!
    irregex-match-start irregex-match-end irregex-match-substring
    irregex-search irregex-search/matches irregex-match irregex-match-string
    irregex-replace irregex-replace/all
    irregex-fold
    irregex-dfa irregex-dfa/search irregex-dfa/extract
    irregex-nfa irregex-flags irregex-submatches irregex-lengths irregex-names
    irregex-quote irregex-opt sre->string string->sre
    irregex-search/all irregex-search/all/strings)
  (import
    (except (rnrs) error remove)
    (prefix (only (rnrs) error) rnrs:)
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (xitomatl include)
    (only (xitomatl strings) string-intersperse)
    (only (xitomatl common-unstandard) with-output-to-string))
  
  ;; --- Derick's additions --------------------------------------------------
  
  (define irregex-search/all
    ;;; TODO? Use irregex-fold
    (case-lambda
      [(x str) 
       (irregex-search/all x str 0)]
      [(x str start)
       (irregex-search/all x str start (string-length str))]
      [(x str start end)
       (irregex-search/all x str start end values)]
      [(x str start end proc)
       (let ([irx (irregex x)])
         (let loop ([start start] [accum '()])
           (let ([m (irregex-search irx str start end)])
             (if m
               (loop (irregex-match-end m 0) (cons (proc m) accum))
               (reverse accum)))))]))
  
  (define irregex-search/all/strings
    ;;; TODO? Use irregex-fold
    (case-lambda
      [(x str) 
       (irregex-search/all/strings x str 0)]
      [(x str start)
       (irregex-search/all/strings x str start (string-length str))]
      [(x str start end)
       (irregex-search/all x str start end
         (lambda (m) (irregex-match-substring m 0)))]))
  
  ;; --- Needed for irregex.scm and irregex-utils.scm ------------------------

  (define (error . args)
    (apply rnrs:error "(library (xitomatl irregex))" args))

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
    
  (include/resolve ("xitomatl" "irregex") "irregex.scm")
  (include/resolve ("xitomatl" "irregex") "irregex-utils.scm")
)
