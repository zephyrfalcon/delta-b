;; test-all.scm

(push! *load-path* ".")
(use gauche.test)

(test-start "all")
(load "test-namespace")
(load "test-parser")
(load "test-tokenizer")
(load "test-tools")
(test-end)