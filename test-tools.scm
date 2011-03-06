;; test-tools.scm

(push! *load-path* ".")
(use gauche.test)
(use tools)

(test-start "tools")

(test-section "unique")
(test* "" '(1 2 3 6) (sort (unique '(1 2 3 1 2 3 6 1)) <))

(test-section "string-slice")
(test* "s[1:]"  "bcde" (string-slice "abcde" 1))
(test* "s[1:0]" ""     (string-slice "abcde" 1 0))
(test* "s[-1:]" "e"    (string-slice "abcde" -1))
(test* "s[:-1]" "abcd" (string-slice "abcde" 0 -1))
(test* "s[1:3]" "bc"   (string-slice "abcde" 1 3))
(test* "s[9:]"  ""     (string-slice "abcde" 9))
(test* "s[4:2]"  ""    (string-slice "abcde" 4 2))

(test-end)