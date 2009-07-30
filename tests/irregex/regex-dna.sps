#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (xitomatl irregex)
  (xitomatl include))

;; You can get the input file at:
;;  http://shootout.alioth.debian.org/download/regexdna-input.txt
;; And the expected output file at:
;;  http://shootout.alioth.debian.org/download/regexdna-output.txt

(define (read-string)
  (get-string-all (current-input-port)))

(include/resolve ("xitomatl" "tests" "irregex") "regex-dna.scm")
