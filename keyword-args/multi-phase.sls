#!r6rs
(library (xitomatl keyword-args multi-phase)
  (export
    kw-arg? make-kw-arg kw-arg-name.value
    process-args
    get-kw-val)
  (import
    (rnrs))

  (define-record-type kw-arg (fields name.value))  
  
  (define (process-args args who default-vals)
    ;; NOTE: Called at run-time and expand-time.
    (if (null? args)
      default-vals
      (let ([arg (car args)]) 
        (if (kw-arg? arg)
          (process-args (cdr args) who (cons (kw-arg-name.value arg) default-vals))
          (assertion-violation who "not a keyword argument" arg)))))
  
  (define (get-kw-val who arg-name default-vals)
    ;; NOTE: Called at run-time and expand-time.
    (if (null? default-vals)
      (assertion-violation who "required keyword argument missing" arg-name)
      (let ([kw.val (car default-vals)])
        (if (eq? arg-name (car kw.val))
          (cdr kw.val)
          (get-kw-val who arg-name (cdr default-vals))))))
  
)
