#!r6rs
(library (xitomatl file-system base)
  (export
    ;; From compat
    current-directory directory-list delete-directory delete-file
    make-directory make-symbolic-link change-mode
    file-exists? file-regular? file-directory? file-symbolic-link?
    ;; This library's things
    directory-walk-enumerator directory-walk directory-walk/choice delete-any)
  (import
    (except (rnrs) file-exists?)
    (xitomatl file-system paths)
    (xitomatl file-system base compat)
    (only (xitomatl define extras) define/?)
    (only (xitomatl enumerators) fold/enumerator)
    (only (xitomatl exceptions) warning))
  
  ;; TODO? directory-enumerator which uses FFI to get directory entries as they're
  ;;       needed; contrasted with directory-list which gets them all at once.
  
  (define/? directory-walk-enumerator
    ;; Like all (xitomatl enumerators) enumerators, proc must return as its 
    ;; first return value (out of possibly many return values, the rest of
    ;; which are the seeds passed through the fold) a true value or #f to
    ;; indicate to continue or stop the fold.
    ;; When top-down is chosen, proc's true/continue return value must be a list
    ;; (possibly empty) of the sub-directories (as relative-path strings) it
    ;; wants the walk to descend into.
    ;; When bottom-up is chosen, proc's indicator return value can be anything.
    (case-lambda/?
      [()
       (directory-walk-enumerator 'top-down)]
      [([way top-down/bottom-up?])
       (lambda/? ([start-path path?] proc seeds)
         (define (dir-contents path)
           (call/cc
            (lambda (k)
              (let loop ([l (with-exception-handler
                              (lambda (ex)
                                (unless (i/o-filename-error? ex)
                                  (raise ex))
                                (warning  ;; does raise-continuable
                                 'directory-walk-enumerator
                                 "Exception raised from directory walking"
                                 path
                                 (if (condition? ex) (simple-conditions ex) ex))
                                ;; Continuing not currently working with PLT's
                                ;; broken R6RS exceptions implementation.
                                (k #f #f #f #f))
                              (lambda () (directory-list (path-join abs path))))]
                         [d '()] [f '()] [s '()])
                (if (null? l)
                  (values d f s #t)
                  (let* ([n (car l)]
                         [abs-n (path-join abs path n)])
                    (case (guard (ex [else 'other])
                            (cond [(file-regular? abs-n #f) 'file]
                                  [(file-directory? abs-n #f) 'directory]
                                  [(file-symbolic-link? abs-n) 'symbolic-link]
                                  [else 'other]))
                      [(file other)
                       (loop (cdr l) d (cons n f) s)]
                      [(directory)
                       (loop (cdr l) (cons n d) f s)]
                      [(symbolic-link)
                       (loop (cdr l) d f (cons n s))])))))))
         ;; Designed to be tail-recursive for both top-down and bottom-up.
         (define (walk paths revisit seeds)
           (cond 
             [(null? paths)
              (assert (or (not revisit) (null? revisit)))
              (apply values seeds)]
             [(null? (car paths))
              (if (pair? revisit)  ;; only true when bottom-up
                (let-values ([(cont . next-seeds) 
                              (apply proc (append (car revisit) seeds))])
                  (if cont
                    (walk (cdr paths) (cdr revisit) next-seeds)
                    (apply values next-seeds)))
                (walk (cdr paths) revisit seeds))]
             [else
              (let ([p (caar paths)]
                    [rest (cons (cdar paths) (cdr paths))])
                (let-values ([(dirs files syms ok) (dir-contents p)])
                  (if ok
                    (do-way rest revisit p dirs files syms seeds)
                    (walk rest revisit seeds))))]))
         (define (top-down rest _ path dirs files syms seeds)
           (let-values ([(descend . next-seeds) (apply proc path dirs files syms seeds)])
             (if descend
               (begin (assert (and (list? descend) (for-all relative-path? descend)))
                      (walk (cons (map (lambda (x) (path-join path x))
                                       descend)
                                  rest)
                            #f 
                            next-seeds))
               (apply values next-seeds))))
         (define (bottom-up rest revisit path dirs files syms seeds)
           (if (null? dirs)
             (let-values ([(cont . next-seeds) (apply proc path dirs files syms seeds)])
               (if cont
                 (walk rest revisit next-seeds)
                 (apply values next-seeds)))
             (walk (cons (map (lambda (x) (path-join path x))
                              dirs)
                         rest)
                   (cons (list path dirs files syms) 
                         revisit)
                   seeds)))
         (define do-way
           (case way
             [(top-down) top-down]
             [(bottom-up) bottom-up]))
         (define abs
           ;; Get the absolute base path of now to use above so we are not vulnerable
           ;; to the current-directory changing while we are enumerating/folding.
           (if (absolute-path? start-path)
             (root-dir-str)
             (current-directory)))
         (walk (list (list start-path)) '() seeds))]))
  
  (define (top-down/bottom-up? x)
    (memq x '(top-down bottom-up)))
  
  (define/? directory-walk
    ;; When 'top-down is chosen, all sub-directories are descended into.
    (case-lambda/?
      [(proc path) 
       (directory-walk proc path 'top-down)]
      [([proc procedure?] path way)
       (fold/enumerator
        (directory-walk-enumerator way)
        path
        (lambda (path dirs files syms)
          (proc path dirs files syms)
          dirs))]))
  
  (define/? (directory-walk/choice [proc procedure?] path)
    (fold/enumerator
     (directory-walk-enumerator 'top-down)
     path
     (lambda (path dirs files syms)
       (let ([descend (proc path dirs files syms)])
         (assert descend) ;; further type check done by directory-walk-enumerator
         descend))))
    
  (define delete-any 
    (case-lambda
      [(path)
       (delete-any path #f)]
      [(path want-error)
       (guard (ex [(and (i/o-filename-error? ex) (not want-error))
                   #f])
         (cond 
           [(file-directory? path #f)
            (directory-walk
             (lambda (p dirs files syms)
               (for-each (lambda (l) 
                           (for-each (lambda (x) (delete-file (path-join p x)))
                                     l))
                         (list files syms))
               (for-each (lambda (x) (delete-directory (path-join p x) #t))
                         dirs))
             path
             'bottom-up)
            (delete-directory path #t)]
           [else 
            (delete-file path)])
         (if want-error (values) #t))]))
  
)
