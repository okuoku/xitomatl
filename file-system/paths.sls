#!r6rs
(library (xitomatl file-system paths)
  (export
    dir-sep-char dir-sep-str root-dir-str
    path? absolute-path? relative-path? path=? 
    path-join path-split cleanse-path)
  (import
    (rnrs)
    (xitomatl strings))
  
  ;; If anyone ever wants to use my libraries on Windows,
  ;; we can cond-expand or something this library to work.
  
  (define dir-sep-char #\/)
  (define dir-sep-str (string dir-sep-char))
  (define (root-dir-str) dir-sep-str)  ;; for Windows, could be parameter, to change drives
  
  (define (starts-with-root? p)
    #;(string=? (root-dir-str) (substring p 0 (string-length (root-dir-str))))
    (char=? dir-sep-char (string-ref p 0)))
  
  (define (path? x)
    (and (string? x) (positive? (string-length x))))
  
  (define (absolute-path? x)
    (and (path? x) (starts-with-root? x)))
  
  (define (relative-path? x)
    (and (path? x) (not (starts-with-root? x))))
  
  (define (path=? x y . r)
    (apply string=? (map cleanse-path (cons* x y r))))
  
  (define (path-join . ps)
    (let ([r (string-intersperse (apply append (map _path-split ps)) 
                                 dir-sep-str)])
      (if (and (not (null? ps))
               (absolute-path? (car ps)))  ;; Windows version wouldn't do this, just return r
        (string-append dir-sep-str r)
        r)))
  
  (define (_path-split p)
    (string-split p dir-sep-str #f))
  
  (define (path-split p)
    (let ([r (_path-split p)])
      (if (absolute-path? p)  ;; Windows version wouldn't do this, just return r
        (cons dir-sep-str r)
        r)))
  
  (define (cleanse-path p)
    (apply path-join (path-split p)))
  
)
