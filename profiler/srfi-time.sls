#!r6rs
(library (xitomatl profiler srfi-time)
  (export
    case-lambda/profiled lambda/profiled define/profiled
    generate-report print-report
    reset-recorded-uses)
  (import
    (rnrs)
    (xitomatl srfi time)
    (xitomatl profiler meta)
    (only (xitomatl box) box-value)
    (only (xitomatl common-unstandard) fprintf pretty-print format))
  
  (def--case-lambda/profiled case-lambda/profiled 
    current-time add-duration time-difference)
  
  (def--lambda/profiled lambda/profiled
    current-time add-duration time-difference)
  
  (def--define/profiled define/profiled 
    current-time add-duration time-difference)
  
  (define (generate-report)
    (let-values ([(keys vals) (hashtable-entries profiled-procedures-HT)])
      (vector->list vals)))
  
  (define print-report
    (case-lambda
      [() (print-report (generate-report))]
      [(report) (print-report report (current-output-port))]
      [(report port)
       (define (fpf str . args) (apply fprintf port str args))
       (define (fpp x) (pretty-print x port))
       (for-each
         (lambda (pp)
           (fpf "\n=================================================================\n")
           (fpf "Profile for:\n")
           (fpp (profiled-procedure-source-code pp))
           (fpf "Statistics:\n")
           (fpf " calls: ~s   returns: ~s   entries/exits: ~s\n"
                (box-value (profiled-procedure-calls-num pp)) 
                (box-value (profiled-procedure-returns-num pp))
                (box-value (profiled-procedure-entries/exits-num pp)))
           (let ([uses (profiled-procedure-uses pp)])
             (define (count get pred)
               (let ([nl '()])
                 (for-each (lambda (u)
                             (let ([n (get u)])
                               (unless (or (not (pred u)) (member n nl))
                                 (set! nl (cons n nl))))) 
                           uses)
                 (apply string-append (map (lambda (n) (format " ~s" n)) (list-sort < nl)))))
             (let ([cs (count procedure-use-args-num procedure-use-called?)])
               (when (positive? (string-length cs))
                 (fpf " numbers of arguments to calls:~a\n" cs)))
             (let ([cs (count procedure-use-retvals-num procedure-use-returned?)])
               (when (positive? (string-length cs))
                 (fpf " numbers of values returned:~a\n" cs)))
             (let ([ts (map (lambda (u)
                              (let* ([d (time-difference (procedure-use-stop u) 
                                                         (procedure-use-start u))]
                                     [t (+ (time-second d) (/ (time-nanosecond d) 1e9))])
                                (max t 0)))
                            uses)])
               (when (positive? (length ts))
                 (fpf " average time: ~s sec\n" 
                      (let loop ([ts^ ts] [a 0])
                        (if (null? ts^) 
                          (/ a (length ts))
                          (loop (cdr ts^) (+ a (car ts^))))))
                 (fpf " minimum time: ~s sec\n" 
                      (let loop ([ts (cdr ts)] [m (car ts)])
                        (if (null? ts) m (loop (cdr ts) (min m (car ts))))))
                 (fpf " maximum time: ~s sec\n" 
                      (let loop ([ts (cdr ts)] [m (car ts)])
                        (if (null? ts) m (loop (cdr ts) (max m (car ts)))))))))
           (fpf "=================================================================\n"))
         report)]))

)
