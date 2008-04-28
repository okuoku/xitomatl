(library (xitomatl engines)
  (export
    make-engine
    engine-return
    mileage)
  (import
    (rnrs)
    (only (ikarus) engine-handler foreign-call void die))    

  ;;; Based off of The Scheme Programming Language engines.
  ;;; NOTE: not currently thread safe
  
  (define-syntax $set-engine-counter!
    ;;; $set-engine-counter! returns the current value of the
    ;;; pcb->engine_counter before setting the new value.
    (syntax-rules ()
      [(_ ticks)
       (foreign-call "ikrt_set_engine_counter" ticks)]))
  
  (define (start-timer ticks)
    ;;; Because of the slight possibility the pcb->engine_counter could run out
    ;;; inbetween when we install timer-handler and set the new ticks below
    ;;; (which would cause timer-handler, and therefore the current do-expire,
    ;;; to be called prematurely), the fuel is manually refilled to ensure 
    ;;; there's enough.
    ($set-engine-counter! 1)
    (engine-handler timer-handler)
    ($set-engine-counter! (fx- ticks)))
  
  (define (reset-state)
    (engine-handler void)
    (set! do-return do-return/oops)
    (set! do-complete do-complete/oops)
    (set! do-expire do-expire/oops))
  
  (define (stop-timer)
    (let ([leftover ($set-engine-counter! 1)])
      (reset-state)
      (abs (fxmin leftover 0))))

  (define (do-return/oops args)
    (apply die 'do-return/oops "internal bug" args))
  (define (do-complete/oops ticks value)
    (die 'do-complete/oops "internal bug" ticks value))
  (define (do-expire/oops resume)
    (die 'do-expire/oops "internal bug" resume))
  
  (define do-return do-return/oops)
  (define do-complete do-complete/oops)
  (define do-expire do-expire/oops)
  
  (define (timer-handler)
    ;;; The pcb->engine_counter just passed 0, so there's definitely
    ;;; enough fuel for do-expire to reset-state.
    (start-timer (call/cc do-expire)))
  
  (define (new-engine resume)
    (define (engine ticks complete expire)
      (unless (and (fixnum? ticks) (fxpositive? ticks))
        (die 'engine "not a positive fixnum" ticks))
      (unless (and (procedure? complete) (procedure? expire))
        (die 'engine "not a procedure" (if (procedure? complete) expire complete)))
      ((call/cc
         (lambda (escape)
           (set! do-return
             (lambda (args)
               (stop-timer)
               (escape (lambda () (apply values args)))))
           (set! do-complete
             (lambda (leftover value)
               (escape (lambda () (complete leftover value)))))
           (set! do-expire
             (lambda (resume-k)
               (reset-state)
               (escape (lambda () (expire (new-engine resume-k))))))
           (resume ticks)))))
    engine)
  
  (define (make-engine proc)
    (unless (procedure? proc)
      (die 'make-engine "not a procedure" proc))    
    (new-engine
      (lambda (ticks)
        (start-timer ticks)
        (let ([value (proc)])
          (let ([leftover (stop-timer)])
            (do-complete leftover value))))))
  
  (define (engine-return . args) (do-return args))
  
  (define (mileage fuel thunk)
    (let loop ([eng (make-engine thunk)] [total-ticks 0])
      (eng fuel
        (lambda (ticks value)
          (+ total-ticks (- fuel ticks)))
        (lambda (new-eng)
          (loop new-eng (+ total-ticks fuel))))))
)
