;;; Blurb...
;;; TODO: need to wait for fasl stuff to be ready so it can be used 
;;; instead of scanning everything to tag or un-tag

;;; NOTE: This library is not currently thread-safe.

(library (xitomatl value-file)  
  (export
    value-file? value-file=?
    vf:base-dir
    vf:new vf:open vf:delete vf:get vf:put!)  
  (import 
    (rnrs)
    (only (ikarus) make-parameter parameterize getenv print-gensym))

  
  (define-record-type value-file (fields (mutable fs-name)))
  
  (define (value-file=? vf1 vf2)
    (and (value-file? vf1) 
         (value-file? vf2)
         (string=? (value-file-fs-name vf1) (value-file-fs-name vf2))))
  
  
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
  
  
  (define vf:base-dir
    (make-parameter 
     (let ([vfb (getenv "VALUE_FILE_BASE")])
       (if (> (string-length vfb) 0)
         vfb
         "./.value-file-base"))
     (lambda (x)
       (unless (string? x) (error 'vf:base-dir "not a string" x))
       x)))
  
  
  (define (complete-path p)
    (string-append (vf:base-dir) "/" p))
  
  
  (define next-name-vf (make-value-file ".next-name"))
  
  (define (next-name)
    (define n (vf:get next-name-vf))
    (vf:put! next-name-vf (+ 1 n))
    (number->string n 16))
  
  
  (define-syntax check-is-string
    (syntax-rules ()
      [(_ who x)
       (unless (string? x)
         (assertion-violation who "not a string" x))]))
  
  
  (define vf:new
    (case-lambda
      [()
       (make-value-file (next-name))]
      [(name)
       (check-is-string 'vf:new name)
       (if (file-exists? (complete-path name))
         (error 'vf:new "file already exists" (complete-path name))
         (make-value-file name))]))
  
  
  (define (vf:open name)
    (check-is-string 'vf:open name)
    (if (file-exists? (complete-path name))
      (make-value-file name)
      (error 'vf:open "file does not exist" (complete-path name))))
  
  
  (define-syntax check-is-value-file
    (syntax-rules ()
      [(_ who x)
       (unless (value-file? x)
         (assertion-violation who "not a value-file" x))]))
  
  
  (define (vf:delete vf)
    (check-is-value-file 'vf:delete vf)
    (with-exception-handler
      (lambda (ex) (raise (condition (make-who-condition 'vf:delete) ex)))
      (lambda () (delete-file (complete-path (value-file-fs-name vf))))))
  
  
  (define (vf:get vf)
    (check-is-value-file 'vf:get vf)
    (call-with-input-file (complete-path (value-file-fs-name vf))
      (lambda (ip)
        (let loop ([x (read ip)] [vals '()])
          (if (eof-object? x)
            (apply values (map replace-tags (reverse vals)))
            (loop (read ip) (cons x vals)))))))  
  
  
  (define (vf:put! vf . v*)
    (check-is-value-file 'vf:put! vf)
    (call-with-port (open-file-output-port 
                      (complete-path (value-file-fs-name vf))
                      (file-options no-fail)
                      (buffer-mode block)
                      (native-transcoder))
      (lambda (op)
        (parameterize (#;[print-graph #t]  ;; pointless because substitute-tags ruins sharing
                       [print-gensym #t])
          (for-each 
            (lambda (v) 
              (write (substitute-tags v) op)
              (write-char #\newline op)) 
            v*)))))
  
  
  (begin   ;; until Ikarus provides directory making functionality
    (unless (file-exists? (vf:base-dir))
      (error '(library (value-file)) "base-dir does not exist" (vf:base-dir)))
    (unless (file-exists? (complete-path (value-file-fs-name next-name-vf)))
      (vf:put! next-name-vf 0)))  
  
)
