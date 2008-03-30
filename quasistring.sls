#!r6rs
;;; Original implementation by Jacob Matthews.
;;; http://people.cs.uchicago.edu/~jacobm/qstr/
;;; Modified to be an R6RS library by Derick Eddington.

(library (xitomatl quasistring)
  (export 
    qs 
    current-quasistring-converter)
  (import 
    (rnrs)
    (xitomatl srfi parameters))
  
  (define current-quasistring-converter 
    (make-parameter 
      display
      (lambda (new)
        (if (procedure? new)
          new
          (assertion-violation 'current-quasistring-converter "not a procedure" new)))))
  
  (define (to-string v)
    (call-with-string-output-port
     (lambda (sop)
       ((current-quasistring-converter) v sop))))
  
  (define-syntax qs
    (lambda (stx)
      
      (define (rewrite val kw)
        
        (define (start)
          (let ((exprs (string->strings-and-syntax (syntax->datum val))))
            (if (pair? (cdr exprs))
              #`(string-append #,@exprs)
              (car exprs))))
        
        (define (port->syntaxes p)
          ; port->syntaxes : input-port -> (listof syntax)          
          (define (port->syntaxes/str acc-str)
            (define (curr-string) (datum->syntax #'ignore (list->string (reverse acc-str))))
            (let ((c (read-char p)))
              (cond
                [(eof-object? c) (list (curr-string))]
                [(eq? c #\$) (cons (curr-string) (port->syntaxes/expr))]
                [(eq? c #\\) (port->syntaxes/escape acc-str)]
                [else (port->syntaxes/str (cons c acc-str))])))
          
          (define (port->syntaxes/escape acc-str)
            (define (curr-string) (datum->syntax #'ignore (list->string (reverse (cons #\\ acc-str)))))
            (let ((c (read-char p)))
              (cond
                [(eof-object? c) (list (curr-string))]
                [(eq? c #\$) (port->syntaxes/str (cons #\$ acc-str))]
                [else (port->syntaxes/str (cons* c #\\ acc-str))])))
          
          (define (port->syntaxes/expr)
            (guard (ex
                    [(lexical-violation? ex) 
                     (raise 
                       (condition 
                         (make-who-condition (syntax->datum kw))
                         (make-message-condition 
                           "invalid lexical syntax inside quasistring expression")
                         ex))])
              (let ([string-expr (read p)])
                (if (eof-object? string-expr)
                  (syntax-violation #f "no expression follows quasistring delimiter" stx)
                  (cons
                   #`(to-string #,string-expr)
                   (port->syntaxes/str '()))))))
          
          (port->syntaxes/str '()))
        
        ; string->strings-and-syntax : string -> listof syntax
        ; given a string, produces a list of syntax objects that when evaluated
        ; produce strings that can be appended to produce the equivalent quasistring value
        (define (string->strings-and-syntax str)
          (port->syntaxes (open-string-input-port str)))
        
        (start))
      
      (syntax-case stx ()
        [(kw s) 
         (string? (syntax->datum #'s))
         (rewrite #'s #'kw)]
        [(_ x) 
         (syntax-violation #f "not a string" stx)])))
)
