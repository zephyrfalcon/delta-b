;; test-parser.scm

(push! *load-path* ".")
(load "parser")
(use gauche.test)

(test-start "parser")

(define t1 '((integer 4) (method-call-name "plus:") (integer 5) (dot ".")))

(test-section "match-literal")
(receive (matched rest)
    (match-literal t1)
  (test* "" '(literal . (integer 4)) matched)
  (test* "" (cdr t1) rest))
            

(test-end)
