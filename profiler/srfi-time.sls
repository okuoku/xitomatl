(library (xitomatl profiler srfi-time)
  (export
    case-lambda/profiled
    lambda/profiled
    define/profiled
    #;generate-report
    #;print-report)
  (import
    (rnrs)
    (xitomatl srfi time)
    (xitomatl profiler meta))
  
  (def--case-lambda/profiled case-lambda/profiled 
    current-time add-duration time-difference)
  
  (def--lambda/profiled lambda/profiled
    current-time add-duration time-difference)
  
  (def--define/profiled define/profiled 
    current-time add-duration time-difference)
  
  #;(define (generate-report)
    ;;
    (map
      (lambda (key)
        (define pp (hashtable-ref profiled-procedures-HT key #f))
        (define uses (profiled-procedure-uses pp))
        (make-call-stats
          (profiled-procedure-source-code pp)
          (length calls)
          ;; calls is in reverse order, so (car calls) is the most recent
          (profiled-call-dyn-ext-entries (car calls))
          (profiled-call-dyn-ext-exits (car calls))
          TODO))
      (vector->list (hashtable-keys profiled-procedures-HT))))
  
  #;(define print-report
    (case-lambda
      [() (print-report (generate-report))]
      [(report) (print-report report (current-output-port))]
      [(report port)
       TODO]))
)
