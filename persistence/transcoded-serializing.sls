#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (xitomatl persistence transcoded-serializing)
  (export
    transcoded-serializer
    transcoded-deserializer)
  (import
    (rnrs))
  
  (define transcoded-serializer 
    (case-lambda
      ((s) (transcoded-serializer s (native-transcoder)))
      ((s t)
       (lambda (x p) (s x (transcoded-port p t))))))
  
  (define transcoded-deserializer
    (case-lambda
      ((ds) (transcoded-deserializer ds (native-transcoder)))
      ((ds t)
       (lambda (p) (ds (transcoded-port p t))))))
  
)
