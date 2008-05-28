#!r6rs
(library (xitomatl persistence transcoded-serializing)
  (export
    transcoded-serializer
    transcoded-deserializer)
  (import
    (rnrs))
  
  (define transcoded-serializer 
    (case-lambda
      [(s) (transcoded-serializer s (native-transcoder))]
      [(s t)
       (lambda (x p) (s x (transcoded-port p t)))]))
  
  (define transcoded-deserializer
    (case-lambda
      [(ds) (transcoded-deserializer ds (native-transcoder))]
      [(ds t)
       (lambda (p) (ds (transcoded-port p t)))]))
  
)
