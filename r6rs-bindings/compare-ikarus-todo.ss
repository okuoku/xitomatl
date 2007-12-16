#!/usr/bin/env scheme-script
(import 
  (ikarus)
  (match)
  (read-line)
  (read-all)
  (r6rs-bindings helpers))

(define todo-r6rs.ss
  (call-with-input-file
      (string-append (getenv "HOME") "/ikarus.dev/scheme/todo-r6rs.ss")
    (lambda (p)
      (read-line p)  ;; discard first hash-bang line
      (read-all p))))

(define todo/library-names
  ; ([<code-name> <lib-name>] ...)
  (match todo-r6rs.ss
    [(,import-form
      (define library-names ',value)
      . ,rest)
     value]))

(define LN->CN
  (let ([alist (map (lambda (x) (cons (cadr x) (car x)))
                    todo/library-names)])
    (lambda (lib-name)
      (cond [(assoc lib-name alist) => cdr]
            [(assoc (list lib-name) alist) => cdr]
            [else (error 'LN->CN "internal error" lib-name)]))))

(define todo/identifier-names
  ; ([<identifier> <completed?> <code-name> ...] ...)
  (match todo-r6rs.ss
    [(,import-form
      ,def-library-names
      ,def-status-names
      (define identifier-names ',value)
      . ,rest)
     value]))

;;; todo has all libraries?
(define todo/has-libs 
  '())
(let ([todo/lns (map cadr todo/library-names)])
  (display "todo does not include libraries:\n")
  (for-each (lambda (ln)
              (if (or (member ln todo/lns)
                      (member (list ln) todo/lns))
                (set! todo/has-libs (append todo/has-libs (list ln)))
                (printf " ~s\n" ln)))
            (all-libs)))

;;; todo has all identifiers?
(let ([todo/ids (map car todo/identifier-names)])
  (display "todo is missing identifiers:\n")
  (for-each (lambda (id)
              (unless (member id todo/ids)
                (printf " ~s\n" id)))
            (all-ids)))

;;; todo's identifiers are properly classified?
(let ([lib+ids 
       (filter (lambda (l+ids)
                 (member (car l+ids) todo/has-libs))
               (map (lambda (ln+bs)
                      (cons (car ln+bs)
                            (map binding-name (cdr ln+bs))))
                    (read-all-bindings-specs)))])
  (printf "todo identifiers not properly classified:\n")
  (for-each (lambda (l+ids)
              (define lib-cn (LN->CN (car l+ids)))
              (for-each (lambda (id)
                          (cond [(assoc id todo/identifier-names)
                                 => (lambda (p)
                                      (define cns (cddr p))
                                      (unless (member lib-cn cns)
                                        (printf " ~s\n" id)
                                        (printf "  should be in: ~s -> ~s\n" (car l+ids) lib-cn)))]))
                        (cdr l+ids)))
            lib+ids))
