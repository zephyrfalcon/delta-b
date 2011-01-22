;; test-parser.scm

(push! *load-path* ".")
(load "parser")
(use gauche.test)

(test-start "parser")

(define t1 '((integer 4) (method-call-name "plus:") (integer 5) (dot ".")))
(define t2 '((identifier "List") (method-call-name "new:") (integer 1)
             (integer 2) (integer 3) (method-call-name "println:")
             (dot ".")))
(define t3 '((integer 5) (method-call-name "println:") (dot ".")))

(test-section "match-literal")
(receive (matched rest)
    (match-literal t1)
  (test* "" '(literal . (integer 4)) matched)
  (test* "" (cdr t1) rest))

(test-section "match-method-call")

(receive (matched rest)
    (match-method-call (cdr t1))
  (test* "" '(method-call . ("plus:" . ((literal . (integer 5))))) matched)
  (test* "" '((dot ".")) rest))

(receive (matched rest)
    (match-method-call (cdr t2))
  (test* "" '(method-call . ("new:" (literal integer 1)
                             (literal integer 2) (literal integer 3)))
         matched))

(receive (matched rest)
    (match-method-call (cdr t3))
  (test* "" '(method-call . ("println:" . ())) matched))

(test-section "match-expression")

(receive (matched rest)
    (match-expression t1)
  (test* "" '(literal . (integer 4)) matched))

(test-end)
