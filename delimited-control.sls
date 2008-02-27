;; By Derick Eddington.
;; Taken from Oleg's http://okmij.org/ftp/Scheme/delim-control-n.scm
;; Modified to parameterize with syntax at expansion-time vs. with 
;; values at run-time (for speed); and modified to use thread-local
;; storage parameters instead of a global variable (for multi-threaded
;; use); and modified to minimize uses of hole-push!, hole-pop!, and
;; retrieving the parameter-value of hole (for speed).
;; This is pure R6RS except the use of the implementation's 
;; thread-local parameters and enhanced define-syntax*.
;; NOTE: Ikarus does not yet have threads.  Depending on what type of 
;; concurrency Ikarus ends up providing, the way the holes parameter is
;; used might need to change.

(library (xitomatl delimited-control)
  (export 
    abort prompt control shift reset prompt0 control0 shift0 reset0)
  (import 
    (except (rnrs) define-syntax)
    (only (xitomatl define extras) define-syntax)
    (rnrs mutable-pairs)
    (xitomatl srfi parameters))
  
  (define holes (make-parameter '()))
  (define (hole-push! hole) (holes (cons hole (holes))))
  (define (hole-pop!) 
    (let* ([hs (holes)]
           [hole (car hs)]) 
      (holes (cdr hs)) 
      hole))
  #;(define (hole-peek) (car (holes)))

  (define (cell-new v mark) (cons v mark))
  (define (cell-ref c) (car c))
  (define (cell-marked? c) (cdr c))
  (define (cell-remark c mark) (set-cdr! c mark))
  
  ; Essentially this is the ``return from the function''
  (define (abort-top! v) ((cell-ref (hole-pop!)) v))

  (define-syntax (define-unwind-till-marked! stx)
    (syntax-case stx ()
      [(_ ident should-keep)
       (with-syntax ([marked-action 
                      (case (syntax->datum #'should-keep)
                        [(keep)
                         #''()]  ; don't do anything, leave hole alone, keep delimiter
                        [(dont-keep)
                         #'(begin (cell-remark hole #f) '())])]) ; make the hole non-delimiting
         #'(define (ident)
             (let* ([hs (holes)]
                    [hole (if (null? hs)
                            (error 'ident "No prompt set")
                            (car hs))])  ; peek at the top hole
               (if (cell-marked? hole)	; if marked, it's prompt's hole
                 marked-action
                 (begin (holes (cdr hs)) ; remove hole from the top of stack 
                        (cons hole (ident)))))))]))
  
  (define-unwind-till-marked! utm!/keep keep)
  (define-unwind-till-marked! utm!/dont-keep dont-keep)

  (define-syntax (define-control* stx)
    (syntax-case stx ()
      [(_ ident is-shift should-keep)
       (with-syntax ([utm! (case (syntax->datum #'should-keep)
                             [(keep) #'utm!/keep]
                             [(dont-keep) #'utm!/dont-keep])]
                     [sv (case (syntax->datum #'is-shift)
                             [(shift) #'#t]
                             [(not-shift) #'#f])])
         #'(define (ident f)
             (call/cc
              (lambda (k-control)
                (let* ((holes-prefix (reverse (utm!)))
                       (invoke-subcont
                        (lambda (v)
                          (call/cc
                           (lambda (k-return)
                             (hole-push! (cell-new k-return sv))
                             (for-each hole-push! holes-prefix)
                             (k-control v))))))
                  (abort-top! (f invoke-subcont)))))))]))

  (define (prompt* thunk)
    (call/cc
     (lambda (outer-k)
       (hole-push! (cell-new outer-k #t)) ; it's prompt's hole
       (abort-top! (thunk)))))
  
  (define-control* control* not-shift keep)
  
  (define (abort v) (control* (lambda (k) v)))
  
  ; Some syntactic sugar
  
  (define-syntax prompt
    (syntax-rules ()
      [(_ e) (prompt* (lambda () e))]))
  
  (define-syntax control
    (syntax-rules ()
      ((_ k e) (control* (lambda (k) e)))))
  
  ; introduce convenient synonyms  

  (define-syntax reset
    (syntax-rules ()
      [(_ e) (prompt e)]))  
  
  (define-control* shift* shift keep)
  
  (define-syntax shift
    (syntax-rules ()
      [(_ k e) (shift* (lambda (k) e))]))
  

  (define-syntax prompt0
    (syntax-rules ()
      [(_ e) (prompt e)]))
  
  (define-control* control0* not-shift dont-keep)
  
  (define-syntax control0
    (syntax-rules ()
      [(_ k e) (control0* (lambda (k) e))]))

  
  (define-syntax reset0
    (syntax-rules ()
      [(_ e) (prompt e)]))  
  
  (define-control* shift0* shift dont-keep)
  
  (define-syntax shift0
    (syntax-rules ()
      [(_ k e) (shift0* (lambda (k) e))]))
)
