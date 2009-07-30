#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (xitomatl bytevectors)
  (srfi :78 lightweight-testing))

(check (bytevector) 
       => #vu8())
(check (bytevector 1) 
       => #vu8(1))
(check (bytevector 1 2 3 4) 
       => #vu8(1 2 3 4))
(check (bytevector 1 2 3 4 5 42 1 2 3 4 1 2 3 255 123) 
       => #vu8(1 2 3 4 5 42 1 2 3 4 1 2 3 255 123))

(check (bytevector-append)
       => #vu8())
(check (bytevector-append #vu8())
       => #vu8())
(check (bytevector-append #vu8() #vu8())
       => #vu8())
(check (bytevector-append #vu8() #vu8() #vu8() #vu8())
       => #vu8())
(check (bytevector-append #vu8(1))
       => #vu8(1))
(check (bytevector-append #vu8(1 2 3 4 5))
       => #vu8(1 2 3 4 5))
(check (bytevector-append #vu8(1 2 3 4 5) #vu8(1 2 3 4) #vu8(1 2 3))
       => #vu8(1 2 3 4 5 1 2 3 4 1 2 3))
(check (bytevector-append #vu8() #vu8(1 2 3 4 5) #vu8(42) #vu8(1 2 3 4) 
                          #vu8() #vu8() #vu8(1 2 3) #vu8() #vu8(255 123))
       => #vu8(1 2 3 4 5 42 1 2 3 4 1 2 3 255 123))

(check (subbytevector #vu8() 0 0)
       => #vu8())
(check (subbytevector #vu8(1 2 3 4 5) 0 0)
       => #vu8())
(check (subbytevector #vu8(1 2 3 4 5) 0 1)
       => #vu8(1))
(check (subbytevector #vu8(1 2 3 4 5) 3 4)
       => #vu8(4))
(check (subbytevector #vu8(1 2 3 4 5) 1 4)
       => #vu8(2 3 4))
(check (subbytevector #vu8(1 2 3 4 5) 0 5)
       => #vu8(1 2 3 4 5))

(check-report)
