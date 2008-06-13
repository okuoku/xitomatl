#!r6rs
(library (xitomatl characters)
  (export
    char-line-ending?)
  (import
    (rnrs)
    (only (xitomatl define extras) define/?))
  
  (define/? (char-line-ending? [c char?])
    (case c
      [(#\xa #\xd #\x85 #\x2028) #t]
      [else #f]))
  
)
