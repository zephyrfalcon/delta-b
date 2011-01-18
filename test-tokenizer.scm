;; test-tokenizer.scm

(push! *load-path* ".")
(use tokenizer)
(use gauche.test)

(test-start "tokenizer")

(test-module 'tokenizer)

(test* "" '((integer 42)) (tokenize "42"))
(test* "" '((string "\"hello world\"")) (tokenize " \"hello world\"  "))

(test-end)

