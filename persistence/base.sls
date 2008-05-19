#!r6rs
(library (xitomatl persistence base)
  (export
    make-value-directory-fuego-object)
  (import
    (rnrs)
    (only (xitomatl define extras) define/AV)
    (xitomatl file-system paths)
    (xitomatl file-system value-file)
    (xitomatl fuego))
  
  (define value-directory-prototype
    (object
      (method ,:unknown 
        (case-lambda
          [(self resend filename)
           (cond 
             [(and (relative-path? filename)
                   (assoc filename (value-directory-ref (send self 'value-directory))))
              => (lambda (filename.value-thing)
                   (let ([vt (cdr filename.value-thing)])
                     (cond [(value-file? vt) vt]
                           [(value-directory? vt) (make-value-directory-fuego-object vt)]
                           [else (assertion-violation 'value-directory-prototype-:unknown
                                   "internal bug: not a file or directory" self vt)])))]
             [else (resend :unknown filename)])]
          [(self resend filename val)
           (if (relative-path? filename)
             (cond 
               #;[(fuego-object? val)
                (value-file-set! 
                  (make-value-file 
                    (path-join (value-directory-path (send self 'value-directory))
                               filename)) 
                  (send val 'value-directory))]
               [else
                (value-file-set! (make-value-file 
                                  (path-join (value-directory-path (send self 'value-directory))
                                             filename))
                                 val)])
             (resend :unknown filename))]))))
  
  (define/AV (make-value-directory-fuego-object vd/p)
    (object (parent value-directory-prototype)
            (value 'value-directory 
                   (cond [(value-directory? vd/p) vd/p]
                         [(absolute-path? vd/p) (make-value-directory vd/p)]
                         [else (AV "not a value-directory or absolute-path" vd/p)]))))
  
)
