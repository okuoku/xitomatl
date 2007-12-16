#!/usr/bin/env scheme-script
(import 
  (ikarus)
  (match)
  (read-line)
  (read-all)
  (r6rs-bindings helpers))

(define makefile.ss
  (call-with-input-file
      (string-append (getenv "HOME") "/ikarus.dev/scheme/makefile.ss")
    (lambda (p)
      (read-line p)  ;; discard first hash-bang line
      (read-all p))))

(define makefile/library-legend
  ; ([<code-name> <lib-name> <visible?> <required?>] ...)
  (match makefile.ss
    [((import . ,im-rest) . ,(rest1))
     rest1]
    [(,def-scheme-library-files
      ,def-ikarus-system-macros
      (define library-legend ',value)
      . ,rest2)
     value]))

(define LN->CN
  (let ([alist (map (lambda (x) (cons (cadr x) (car x)))
                    makefile/library-legend)])
    (lambda (lib-name)
      (cond [(assoc lib-name alist) => cdr]
            [(assoc (list lib-name) alist) => cdr]
            [else (error 'LN->CN "internal error" lib-name)]))))

(define makefile/identifier->library-map
  ; ([<identifier> <code-name> ...] ...)
  (match makefile.ss
    [((import . ,im-rest) . ,(rest1))
     rest1]
    [(,def-scheme-library-files
      ,def-ikarus-system-macros
      ,def-library-legend
      (define identifier->library-map ',value)
      . ,rest2)
     value]))

(define (spec-name->MF lib-name)
  (if (list? lib-name)
    ; remove version spec from library name
    (let loop ([l lib-name] [n '()])
      (if (or (null? l) (list? (car l)))
        (reverse n) 
        (loop (cdr l) (cons (car l) n))))
    (case lib-name
      [(null-environment)
       '(psyntax null-environment-5)]
      [(scheme-report-environment)
       '(psyntax scheme-report-environment-5)]
      [else lib-name])))

;;; makefile has all libraries?
(define makefile/has-libs 
  '())
(let ([makefile/lns (map cadr makefile/library-legend)])
  (display "makefile does not include libraries:\n")
  (for-each (lambda (ln)
              (if (or (member ln makefile/lns)
                      (member (list ln) makefile/lns))
                (set! makefile/has-libs (append makefile/has-libs (list ln)))
                (printf " ~s\n" ln)))
            (map spec-name->MF (all-libs))))

;;; todo has all identifiers?
(let ([makefile/ids (map car makefile/identifier->library-map)])
  (display "makefile is missing identifiers:\n")
  (for-each (lambda (id)
              (unless (member id makefile/ids)
                (printf " ~s\n" id)))
            (all-ids)))

;;; todo's identifiers are properly classified?
(let ([lib+ids 
       (filter (lambda (l+ids)
                 (member (car l+ids) makefile/has-libs))
               (map (lambda (ln+bs)
                      (cons (spec-name->MF (car ln+bs))
                            (map binding-name (cdr ln+bs))))
                    (read-all-bindings-specs)))])
  (printf "make identifiers not properly classified:\n")
  (for-each (lambda (l+ids)
              (define lib-cn (LN->CN (car l+ids)))
              (for-each (lambda (id)
                          (cond [(assoc id makefile/identifier->library-map)
                                 => (lambda (p)
                                      (define cns (cdr p))
                                      (unless (member lib-cn cns)
                                        (printf " ~s\n" id)
                                        (printf "  should be in: ~s -> ~s\n" (car l+ids) lib-cn)))]))
                        (cdr l+ids)))
            lib+ids))
