;; test-parser.scm

(push! *load-path* ".")
(use tokenizer)
(use parser)
(use tools)
(use gauche.test)

(test-start "parser")

(define t1 '((integer 4) (method-call-name "plus:") (integer 5) (dot ".")))
(define t2 '((identifier "List") (method-call-name "new:") (integer 1)
             (integer 2) (integer 3) (method-call-name "println:")
             (dot ".")))
(define t3 '((integer 5) (method-call-name "println:") (dot ".")))
(define t4 '((lparen "(") (integer 4) (method-call-name "plus:")
             (integer 5) (rparen ")")))
(define t5 (cons '(lbrace "{")
                 (append t1 t2 t3 '((rbrace "}")))))

(define t6 (tokenize "3 plus: 4."))

;;;
(test-section "match-literal")
(receive (matched rest)
    (match-literal t1)
  (test* "" '(literal . (integer 4)) matched)
  (test* "" (cdr t1) rest))

;;;
(test-section "_match-zero-or-more")
(receive (matched rest)
    (_match-zero-or-more match-literal t1)
  (test* "" '((literal integer 4)) matched)
  (test* "" (cdr t1) rest))
(receive (matched rest)
    (_match-zero-or-more match-literal (cddr t2))
  (test* "" '((literal integer 1) (literal integer 2) (literal integer 3))
         matched))

;;;
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

;;;
;; XXX use accessors to make it more clear what is matched
(test-section "match-method-call-chain")
(receive (matched rest)
    (match-method-call-chain t1)
  (test* "" '(method-call-chain
              . ((literal integer 4)
                 . ((method-call . ("plus:" (literal integer 5))))))
         matched)
  (test* "" '((dot ".")) rest))

;;;
(test-section "match-expression")

(receive (matched rest)
    (match-expression t1)
  (test* "" '(literal . (integer 4)) matched))

;;;
(test-section "match-statement")
(receive (matched rest)
    (match-statement t1)
  (test* "" #t (ast-method-call-chain? matched))
  (test* "" '(literal integer 4) (ast-method-call-chain-head matched))
  (test* "" '((method-call "plus:" (literal integer 5)))
         (ast-method-call-chain-calls matched))
  (test* "" '() rest))

;;;
(test-section "match-parenthesized-statement")
(receive (matched rest)
    (match-parenthesized-statement t4)
  (test* "" #t (ast-method-call-chain? matched))
  ;; XXX add more tests here using accessors...
  (test* "" '() rest))

;;;
(test-section "match-block")
(receive (matched rest)
    (match-block t5)
  (test* "number of stmts in block"
         3 (length (ast-block-statements matched)))
  (test* "" '() rest))

;;;
(test-section "match-program")
(let ((stmts (match-program t1)))
  (test* "number of statements" 1 (length stmts))
  (test* "first stmt is method call chain?"
         #t (ast-method-call-chain? (car stmts)))
  )
(let ((stmts (match-program t6)))
  (test* "number of statements" 1 (length stmts))
  (test* "first stmt is method call chain?"
         #t (ast-method-call-chain? (car stmts)))
  )


(test-end)
