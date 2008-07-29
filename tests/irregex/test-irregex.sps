#!r6rs
(import 
  (rnrs)
  (xitomatl irregex)
  (xitomatl match)
  (only (xitomatl strings) string-split)
  (only (xitomatl common-unstandard) format)
  (only (xitomatl ports) port-for-each)
  (xitomatl tests irregex test))

;; Derick Eddington modified this, from Alex Shinn's original, to be R6RS

(define (warning msg . irrts)
  (raise (condition (make-warning) 
                   (make-message-condition msg)
                   (make-irritants-condition irrts))))

(define (subst-matches matches subst)
  (define (submatch n)
    (if (vector? matches)
        (irregex-match-substring matches n)
        (list-ref matches n)))
  (and
   matches
   (call-with-string-output-port 
     (lambda (out)
       (call-with-port (open-string-input-port subst)
         (lambda (in)
           (let lp ()
             (let ((c (read-char in)))
               (cond
                ((not (eof-object? c))
                 (case c
                   ((#\&)
                    (display (or (submatch 0) "") out))
                   ((#\\)
                    (let ((c (read-char in)))
                      (if (char-numeric? c)
                          (display
                           (or (submatch (string->number (string c))) "")
                           out)
                          (write-char c out))))
                   (else
                    (write-char c out)))
                 (lp)))))))))))

(define (test-re matcher line)
  (match (string-split line "\t" #t)
    [(pattern input result subst output)
     (let ((name (format "~a  ~a  ~a" pattern input result)))
       (cond
         ((equal? "c" result)
          (test-error name (matcher pattern input)))
         ((equal? "n" result)
          (test-assert name (not (matcher pattern input))))
         (else
          (test name output
                (subst-matches (matcher pattern input) subst)))))]
    [else
     (warning "invalid regex test line" line)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for-each
 (lambda (opts)
   (test-group (format "irregex - ~s" opts)
       (with-input-from-file "re-tests.txt"
         (lambda ()
           (port-for-each
            (lambda (line)
              (test-re (lambda (pat str)
                         (irregex-search (apply irregex pat opts) str))
                       line))
            get-line)))))
 '((small) (fast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pregexp not passing tests.  Not sure if Alex knows this already.
#;(test-group "pregexp"
  (with-input-from-file "re-tests.txt"
    (lambda ()
      (port-for-each
       (lambda (line) (test-re pregexp-match line))
       get-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(test-group "regex"
   (with-input-from-file "re-tests.txt"
     (lambda ()
       (port-for-each
        (lambda (line) (test-re string-search line))
        read-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "utils"
  (test "h*llo world"
      (irregex-replace "[aeiou]" "hello world" "*"))
  (test "h*ll* w*rld"
      (irregex-replace/all "[aeiou]" "hello world" "*")))

(test-end)
(test-exit 251)
