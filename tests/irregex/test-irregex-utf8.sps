#!r6rs
(import
  (rnrs)
  (xitomatl irregex)
  (xitomatl tests irregex test))

(test-begin)

(test-assert (irregex-search "<..>" "<漢字>"))
(test-assert (irregex-search "<.*>" "<漢字>"))
(test-assert (irregex-search "<.+>" "<漢字>"))
(test-assert (not (irregex-search "<.>" "<漢字>")))
(test-assert (not (irregex-search "<...>" "<漢>")))

(test-assert (irregex-search "<[^a-z]*>" "<漢字>"))
(test-assert (not (irregex-search "<[^a-z]*>" "<漢m字>")))
(test-assert (irregex-search "<[^a-z][^a-z]>" "<漢字>"))
(test-assert (irregex-search "<あ*>" "<あ>"))
(test-assert (irregex-search "<あ*>" "<ああ>"))
(test-assert (not (irregex-search "<あ*>" "<あxあ>")))

(test-assert (irregex-search "<[あ-ん]*>" "<あん>"))
(test-assert (irregex-search "<[あ-ん]*>" "<ひらがな>"))
(test-assert (not (irregex-search "<[あ-ん]*>" "<ひらgがな>")))

(test-end)
(test-exit 14)

