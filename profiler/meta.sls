(library (xitomatl profiler meta)
  (export
    def--case-lambda/profiled def--lambda/profiled def--define/profiled
    case-lambda/profiled--meta
    profiled-procedure?
    profiled-procedure-proc-obj
    profiled-procedure-source-code
    profiled-procedure-calls-num 
    profiled-procedure-returns-num 
    profiled-procedure-entries/exits-num 
    profiled-procedure-uses  ;; In reverse order: newest first
    procedure-use?
    procedure-use-start 
    procedure-use-stop 
    procedure-use-called? 
    procedure-use-args-num 
    procedure-use-returned?
    procedure-use-retvals-num
    profiled-procedures-HT)
  (import
    (rnrs)
    (xitomatl box))
  ;; NOTE for future: not currently thread-safe
  
  (define-syntax box-value+1
    (syntax-rules ()
      [(_ id) (box-value-set! id (+ 1 (box-value id)))]))
  
  (define-syntax def--case-lambda/profiled
    (syntax-rules ()
      [(_ name current-info current-info-diff)
       (define-syntax name
         (syntax-rules ()
           [(_ [formals . body] (... ...))
            (case-lambda/profiled--meta 
             '(case-lambda [formals . body] (... ...))
             current-info current-info-diff
             [formals . body] (... ...))]))]))
  
  (define-syntax def--lambda/profiled
    (syntax-rules ()
      [(_ name current-info current-info-diff)
       (define-syntax name
         (syntax-rules ()
           [(_ formals . body)
            (case-lambda/profiled--meta 
             '(lambda formals . body)
             current-info current-info-diff
             [formals . body])]))]))
  
  (define-syntax def--define/profiled
    (syntax-rules ()
      [(_ name current-info current-info-diff)
       (define-syntax name
         (syntax-rules ()
           [(_ (n . formals) . body)
            (define n 
              (case-lambda/profiled--meta 
               '(define (n . formals) . body)
               current-info current-info-diff
               [formals . body]))]))]))
  
  #;(define (calc-adjust current-info current-info-diff)
    ;;; TODO: redo to match new changes
    #;(let ([start #f] [stop #f] [retvals-num #f])
      (dynamic-wind
        (lambda () 
          (set! start (current-info)))
        (lambda ()
          (call-with-values
            (lambda () #f)
            (lambda rv
              (set! returned? #t)
              (set! retvals-num (length rv))
              (apply values rv))))
        (lambda ()
          (set! stop (current-info))))
      (current-info-diff stop start)))
  
  (define-syntax case-lambda/profiled--meta
    (lambda (stx)
      (syntax-case stx ()
        [(_ source-code current-info current-info-diff [formals . body] ...)
         (for-all identifier? (list #'current-info #'current-info-diff))
         #'(let ([calls-num (make-box 0)] 
                 [returns-num (make-box 0)]
                 [entries/exits-num (make-box 0)])
             (define the-proc
               (case-lambda [formals . body] ...))
             (define (profiled-proxy . args)
               (box-value+1 calls-num)
               (let ([adjust #f] [start #f] [stop #f] [retvals-num #f]
                     [called? #t] [returned? #f])
                 (dynamic-wind
                   (lambda () 
                     (box-value+1 entries/exits-num)
                     #;(set! adjust (calc-adjust current-info current-info-diff))
                     (set! start (current-info)))
                   (lambda ()
                     (call-with-values
                       (lambda ()
                         (apply the-proc args))
                       (lambda rv
                         ;; TODO: better adjustment based on whether the-proc returned or not
                         (box-value+1 returns-num)
                         (set! returned? #t)
                         (set! retvals-num (length rv))
                         (apply values rv))))
                   (lambda ()
                     (set! stop (current-info))
                     #;XXX ;; TODO: use calculated adjustment to adjust start (or stop)
                     (record-procedure-use profiled-proxy start stop
                       called? (if called? (length args) #f) 
                       returned? (if returned? retvals-num #f))
                     ;; reset incase a continuation from the-proc is re-entered
                     (set! called? #f)
                     (set! returned? #f)))))
             (register-procedure profiled-proxy source-code 
               calls-num returns-num entries/exits-num)
             profiled-proxy)])))
    
  (define-record-type profiled-procedure
    (fields proc-obj source-code calls-num returns-num entries/exits-num (mutable uses)))
  
  (define-record-type procedure-use
    (fields start stop called? args-num returned? retvals-num))
  
  (define profiled-procedures-HT (make-eq-hashtable))
  
  (define (register-procedure proc source-code calls-num returns-num entries/exits-num)
    (hashtable-set! profiled-procedures-HT proc 
      (make-profiled-procedure proc source-code calls-num returns-num entries/exits-num '())))
  
  (define (record-procedure-use proc start stop called? args-num returned? retvals-num)
    (let ([pp (hashtable-ref profiled-procedures-HT proc #f)])
      (profiled-procedure-uses-set! pp 
        (cons (make-procedure-use start stop called? args-num returned? retvals-num) 
              (profiled-procedure-uses pp)))))  
)
