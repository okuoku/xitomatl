;;; Blurb...
;;; TODO: need to wait for fasl stuff to be ready so it can be used 
;;; instead of scanning everything to tag or un-tag

(library (value-file)
  
  (export
    base-dir
    value-file?
    new get put)
  
  (import (ikarus))
  
  
  (define-record-type value-file (fields (mutable fs-name)))
  
  
  (begin  ; until fasl is used
    (define tag 'W4=!L&K/cQ=2hD<a)  ;;; generated with (uuid)
    
    (define (substitute-tags v)
      (cond 
        [(pair? v)
         (cons (substitute-tags (car v)) (substitute-tags (cdr v)))]
        [(vector? v)
         (vector-map substitute-tags v)]
        [(value-file? v)
         (cons tag (value-file-fs-name v))]
        [else v]))
    
    (define (replace-tags v)
      (cond 
        [(pair? v)
         (if (equal? (car v) tag)
           (make-value-file (cdr v))
           (cons (replace-tags (car v)) (replace-tags (cdr v))))]
        [(vector? v)
         (vector-map replace-tags v)]
        [else v])))
  
  
  (define base-dir
    (make-parameter 
     (let ([vfb (getenv "VALUE_FILE_BASE")])
       (if (> (string-length vfb) 0)
         vfb
         "./.value-file-base"))
     (lambda (x)
       (unless (string? x) (error 'base-dir "not a string" x))
       x)))
  
  
  (define (complete-path p)
    (string-append (base-dir) "/" p))
  
  
  (define next-name-vf (make-value-file ".next-name"))
  
  (define (next-name)
    (define n (get next-name-vf))
    (put next-name-vf (add1 n))
    (number->string n))
  
  
  (define (new . v*)
    (define n (make-value-file (next-name)))
    (when (positive? (length v*))
      (apply put n v*))
    n)
  
  
  (define (get vf)
    (define fn (complete-path (value-file-fs-name vf)))
    (if (file-exists? fn)      
      (with-input-from-file fn
        (lambda ()
          (let loop ([x (read)] [vals '()])
            (if (eof-object? x)
              (apply values (map replace-tags (reverse vals)))
              (loop (read) (cons x vals))))))
      (void)))
  
  
  (define (put vf . v*)
    (with-output-to-file (complete-path (value-file-fs-name vf))
      (lambda ()
        (for-each (lambda (v) (write (substitute-tags v)) (write-char #\newline)) 
                  (filter (lambda (x) (not (eq? x (void)))) v*)))
      'truncate))
  
  
  (begin   ;; until Ikarus provides directory making functionality
    (unless (file-exists? (base-dir))
      (error '(library (value-file)) "base-dir does not exist" (base-dir)))
    (unless (file-exists? (complete-path (value-file-fs-name next-name-vf)))
      (put next-name-vf 0)))
  
  
)