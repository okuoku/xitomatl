;;; NOTE: The 4 tests failing are known.  I think it's an IrRegex bug.

#!r6rs
(import
  (rnrs)
  (xitomatl irregex)
  (xitomatl srfi lightweight-testing))

(define-syntax check-ex/not-advancing
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (condition-message ex))])
              expr
              'unexpected-return)
            => "pattern not advancing search")]))

(define-syntax check-AV
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (assertion-violation? ex)])
              expr
              'unexpected-return)
            => #t)]))

;;----------------------------------------------------------------------------

(define text0 "This is a sentence.")

;; irregex-search/all

(check (irregex-search/all "foobar" text0) 
       => '())
(check (let ([ms (irregex-search/all "\\w+" text0)])
         (and (for-all irregex-match-data? ms)
              (length ms)))
       => 4)
(check (map irregex-match-substring
            (irregex-search/all "\\w+" text0))
       => '("This" "is" "a" "sentence"))
(check (map (lambda (m) (irregex-match-substring m 1))
            (irregex-search/all "\\w+\\s+(\\w+)" text0))
       => '("is" "sentence"))
(check-ex/not-advancing (irregex-search/all "" text0))
(check-ex/not-advancing (irregex-search/all ".*" text0))
(check-ex/not-advancing (irregex-search/all "x*?" text0))

;; irregex-search/all/strings

(check (irregex-search/all/strings "foobar" text0) 
       => '())
(check (irregex-search/all/strings "\\w+" text0)
       => '("This" "is" "a" "sentence"))
(check (irregex-search/all/strings "\\w+\\s+(\\w+)" text0)
       => '("This is" "a sentence"))
(check-ex/not-advancing (irregex-search/all/strings "" text0))
(check-ex/not-advancing (irregex-search/all/strings ".*" text0))
(check-ex/not-advancing (irregex-search/all/strings "x*?" text0))

;;----------------------------------------------------------------------------

;; pair-chain-chunking-lose-refs! 
;; (Same procedure as list-chunking-lose-refs! and port-chunking-lose-refs!)

(let* ([chunk (list "This" "is" "a" "test" "of" "losing" "refs")]
       [submatch-chunks (list chunk (cddddr chunk)
                              (cdr chunk) (cdddr chunk)
                              (cddr chunk) (cddddr chunk)
                              (cddddr chunk) (cddddr chunk))]
       [replacements (pair-chain-chunking-lose-refs! submatch-chunks)])      
  (check (length chunk) => 5)
  (check (for-all eq? replacements submatch-chunks) => #t)
  (check (list-ref replacements 0) => '("This" "is" "a" "test" "of"))
  (check (list-ref replacements 1) => '("of"))
  (check (list-ref replacements 2) => '("is" "a" "test" "of"))
  (check (list-ref replacements 3) => '("test" "of"))
  (check (list-ref replacements 4) => '("a" "test" "of"))
  (check (list-ref replacements 5) => '("of"))
  (check (list-ref replacements 6) => '("of"))
  (check (list-ref replacements 7) => '("of")))

;;----------------------------------------------------------------------------

(define chunked-text0 
  '("Once u" "" "p" "on " "" "a time." "." ". " 
    " There was a string " "used for tes" "ting ch" "unks!" ""))
(define (list-copy l)
  (map values l))

;; irregex-search/chunked/all

(check (irregex-search/chunked/all "foobar" list-chunker '(""))
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker (list "")
                                   list-chunking-lose-refs!)
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker '("" "" "" ""))
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker (list "" "" "" "")
                                   list-chunking-lose-refs!)
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker chunked-text0)
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker
                                   (list-copy chunked-text0)
                                   list-chunking-lose-refs!)
       => '())
(check (let ([ms (irregex-search/chunked/all "\\w+" list-chunker chunked-text0)])
         (and (for-all irregex-match-data? ms)
              (length ms)))
       => 12)
(check (let ([ms (irregex-search/chunked/all "\\w+" list-chunker 
                                             (list-copy chunked-text0)
                                             list-chunking-lose-refs!)])
         (and (for-all irregex-match-data? ms)
              (length ms)))
       => 12)
(check (map irregex-match-substring
            (irregex-search/chunked/all "\\w+" list-chunker chunked-text0))
       => '("Once" "upon" "a" "time" 
            "There" "was" "a" "string" "used" "for" "testing" "chunks"))
(check (map irregex-match-substring
            (irregex-search/chunked/all "\\w+" list-chunker
                                        (list-copy chunked-text0)
                                        list-chunking-lose-refs!))
       => '("Once" "upon" "a" "time" 
            "There" "was" "a" "string" "used" "for" "testing" "chunks"))
(check (map (lambda (m) (irregex-match-substring m 1))
            (irregex-search/chunked/all "[Oo](\\w)" list-chunker chunked-text0))
       => '("n" "n" "r"))
(check (map (lambda (m) (irregex-match-substring m 1))
            (irregex-search/chunked/all "[Oo](\\w)" list-chunker
                                        (list-copy chunked-text0)
                                        list-chunking-lose-refs!))
       => '("n" "n" "r"))
(check (map (lambda (m) (irregex-match-substring m 4))
            (irregex-search/chunked/all "(e)((\\w+)(e))" list-chunker chunked-text0))
       => '("e"))
(check (map (lambda (m) (irregex-match-substring m 4))
            (irregex-search/chunked/all "(e)((\\w+)(e))" list-chunker
                                        (list-copy chunked-text0)
                                        list-chunking-lose-refs!))
       => '("e"))
(check (map irregex-match-substring
            (irregex-search/chunked/all "^.+$" list-chunker chunked-text0))
       => '("Once upon a time...  There was a string used for testing chunks!"))
(check (map irregex-match-substring
            (irregex-search/chunked/all "^.+$" list-chunker
                                        (list-copy chunked-text0)
                                        list-chunking-lose-refs!))
       => '("Once upon a time...  There was a string used for testing chunks!"))
(check-ex/not-advancing 
 (irregex-search/chunked/all "^.*$" list-chunker chunked-text0))
(check-ex/not-advancing 
 (irregex-search/chunked/all "^.*$" list-chunker
                             (list-copy chunked-text0)
                             list-chunking-lose-refs!))

;; irregex-search/chunked/all/strings

(check (irregex-search/chunked/all/strings "foobar" list-chunker '(""))
       => '())
(check (irregex-search/chunked/all/strings "foobar" list-chunker '("" "" "" ""))
       => '())
(check (irregex-search/chunked/all/strings "foobar" list-chunker chunked-text0)
       => '())
(check (let ([ms (irregex-search/chunked/all/strings "\\w+" list-chunker chunked-text0)])
         (and (for-all string? ms)
              (length ms)))
       => 12)
(check (irregex-search/chunked/all/strings "\\w+" list-chunker chunked-text0)
       => '("Once" "upon" "a" "time" 
            "There" "was" "a" "string" "used" "for" "testing" "chunks"))
(check (irregex-search/chunked/all/strings "[Oo](\\w)" list-chunker chunked-text0)
       => '("On" "on" "or"))
(check (irregex-search/chunked/all/strings "(e)((\\w+)(e))" list-chunker chunked-text0)
       => '("ere"))
(check (irregex-search/chunked/all/strings "^.+$" list-chunker chunked-text0)
       => '("Once upon a time...  There was a string used for testing chunks!"))
(check-ex/not-advancing 
 (irregex-search/chunked/all/strings "^.*$" list-chunker chunked-text0))

;;----------------------------------------------------------------------------
    
(check-AV (make-port-chunker 'oops))
(check-AV (make-port-chunker 0))
(check-AV (make-port-chunker -1))

(define port-chunker-one (make-port-chunker 1))
(define port-chunker-two (make-port-chunker 2))
(define port-chunker-seven (make-port-chunker 7))
(define port-chunker-1e4 (make-port-chunker 10000))
(define (make-sip) 
  (open-string-input-port
   "The best way to implement the future is to avoid having to predict it."))

;; irregex-search-port 
#|
(define sip0 (make-sip))
(check (let ([m (irregex-search-port "\\w+\\s+(\\w+)" sip0)])
         (and m (irregex-match-substring m 1)))
       => "best")
(check (let ([m (irregex-search-port "\\w+\\s+(\\w+)" sip0)])
         (and m (irregex-match-substring m 1)))
       => "to")
(check (let ([m (irregex-search-port "\\w+\\s+(\\w+)" sip0)])
         (and m (irregex-match-substring m 1)))
       => "the")
(check (irregex-search-port "foo bar baz" sip0)
       => #f)
(check (eof-object? (get-char sip0)) => #t)
|#

;; irregex-search-port/all 

(check (map irregex-match-substring 
            (irregex-search-port/all "([Tt]o).*?\\1" (make-sip)))
       => '("to implement the future is to"))
(check (map irregex-match-substring 
            (irregex-search-port/all "\\s" (make-sip)))
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all "([Tt]o).*?\\1" (make-sip)
                                irregex-match-substring port-chunker-one)
       => '("to implement the future is to"))
(check (irregex-search-port/all "\\s" (make-sip) 
                                irregex-match-substring port-chunker-one)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all "([Tt]o).*?\\1" (make-sip)
                                irregex-match-substring port-chunker-two)
       => '("to implement the future is to"))
(check (irregex-search-port/all "\\s" (make-sip) 
                                irregex-match-substring port-chunker-two)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all "([Tt]o).*?\\1" (make-sip)
                                irregex-match-substring port-chunker-seven)
       => '("to implement the future is to"))
(check (irregex-search-port/all "\\s" (make-sip) 
                                irregex-match-substring port-chunker-seven)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all "([Tt]o).*?\\1" (make-sip)
                                irregex-match-substring port-chunker-1e4)
       => '("to implement the future is to"))
(check (irregex-search-port/all "\\s" (make-sip) 
                                irregex-match-substring port-chunker-1e4)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip)))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip) values port-chunker-one))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip) values port-chunker-two))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip) values port-chunker-seven))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip) values port-chunker-1e4))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip)))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip) values 
                                                 port-chunker-one))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip) values 
                                                 port-chunker-two))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip) values 
                                                 port-chunker-seven))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip) values 
                                                 port-chunker-1e4))

;; irregex-search-port/all/strings

(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip))
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip))
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip) port-chunker-one)
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip) port-chunker-one)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip) port-chunker-two)
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip) port-chunker-two)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip) port-chunker-seven)
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip) port-chunker-seven)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip) port-chunker-1e4)
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip) port-chunker-1e4)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip)))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip) port-chunker-one))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip) port-chunker-two))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip) port-chunker-seven))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip) port-chunker-1e4))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip)))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip) 
                                                         port-chunker-one))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip) 
                                                         port-chunker-two))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip) 
                                                         port-chunker-seven))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip) 
                                                         port-chunker-1e4))


;;; TODO: Test memory usage of chunked irregex-search-port and irregex-search-port/all


(check-report)
