;; test-tools.scm

(push! *load-path* ".")
(use gauche.test)
(use tools)

(test-start "tools")

(test-section "unique")
(test* "" '(1 2 3 6) (sort (unique '(1 2 3 1 2 3 6 1)) <))

(test-end)