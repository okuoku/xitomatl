#!r6rs
(library (xitomatl records)
  (export
    record-type-accessors
    record-type-mutators)
  (import
    (rnrs)
    (only (xitomatl define extras) define/?)
    (only (xitomatl indexes) enumerate))
  
  (define (record-type-field-procs rtd field-proc pred)
    (let loop ([rtd rtd] [procs '()])
      (if rtd
        (loop (record-type-parent rtd)
              (cons (map (lambda (i) (and (pred rtd i) (field-proc rtd i)))
                         (enumerate (record-type-field-names rtd)))
                    procs))
        (apply append procs))))
  
  (define/? (record-type-accessors [rtd record-type-descriptor?])
    (record-type-field-procs rtd record-accessor (lambda (rtd i) #t)))
  
  (define/? (record-type-mutators [rtd record-type-descriptor?])
    (record-type-field-procs rtd record-mutator 
                             (lambda (rtd i) (record-field-mutable? rtd i))))
  
)
