;; test-tokenizer.scm

(push! *load-path* ".")
(use tokenizer)
(use gauche.test)

(test-start "tokenizer")

(test-module 'tokenizer)

(test* "" '((integer 42)) (tokenize "42"))
(test* "" '((integer -1)) (tokenize "-1"))
(test* "" '((float 3.1415)) (tokenize "3.1415"))
(test* "" '((string "\"hello world\"")) (tokenize " \"hello world\"  "))
(test* "" '((identifier "foo")) (tokenize "foo"))
(test* "" '((symbol "#zap")) (tokenize "#zap"))

(test* "" '((integer 1) (integer 2) (integer 3)) (tokenize "1 2 3"))
(test* "" '((identifier "foo") (identifier "bar")) (tokenize "foo bar"))
(test* "" '((integer 1) (method-call-name "plus:") (integer 2))
       (tokenize "1 plus: 2"))
(test* "" '((lparen "(") (identifier "a") (rparen ")")) (tokenize "(a)"))
(test* "" '((identifier "~") (method-call-name "bar:") (dot "."))
       (tokenize "~ bar:."))
(test* "" '((lbrace "{") (integer 3) (rbrace "}"))
       (tokenize "{ 3 }"))
(test* "" '((lbrace "{") (integer 3) (rbrace "}") (dot "."))
       (tokenize "{ 3 }."))

(define s1 "4. -- this is a comment")
(test* "" '((integer 4) (dot ".")) (tokenize s1))
(define s2 "4. -- this is a comment\n")
(test* "" '((integer 4) (dot ".")) (tokenize s2))
(define s3 "4. -- this is a comment\n 5")
(test* "" '((integer 4) (dot ".") (integer 5)) (tokenize s3))

(test-end)

