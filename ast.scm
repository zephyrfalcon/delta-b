;; ast.scm

;; AST nodes are represented as pairs. The CAR is the symbol that
;; indicates the type of node; the CDR is its value.

(define (make-ast-node symbol value)
  (cons symbol value))
(define (ast-symbol node)
  (car node))
(define (ast-value node)
  (cdr node))
(define (ast-node? x)
  (pair? x))

;; literals
;; represented as: (literal . <token>)
;; can be: integers, floats, strings, symbols, etc

(define (make-ast-literal token)
  (make-ast-node 'literal token))
(define (ast-literal? node)
  (equal? (car node) 'literal))

;; identifiers
;; represented as: (identifier . <name>)
;; where <name> is a string.
;; XXX this could probably be collapsed into literals?

(define (make-ast-identifier name)
  (make-ast-node 'identifier name))
(define (ast-identifier? node)
  (equal? (car node) 'identifier))

;; blocks
;; represented as: (block . <statement>*)
;; where <statement>* is a list of zero or more statements, all of which are
;; AST nodes themselves.

(define (make-ast-block stmts)
  (make-ast-node 'block stmts))
(define (ast-block? node)
  (equal? (car node) 'block))

;; method calls
;; represented as: (method-call . (<method> . <args>*))
;; where <method> is a string containing the name of the method;
;; <args>* is a list of zero or more arguments (all AST nodes).

(define (make-ast-method-call method args)
  (make-ast-node 'method-call (cons method args)))
(define (ast-method-call? node)
  (equal? (car node) 'method-call))
(define (ast-method-call-method node)
  (cadr node))
(define (ast-method-call-args node)
  (cddr node))

;; method call chains

(define (make-ast-method-call-chain head calls)
  (make-ast-node 'method-call-chain (cons head calls)))
(define (ast-method-call-chain? node)
  (equal? (car node) 'method-call-chain))
(define (ast-method-call-chain-head node)
  (cadr node))
(define (ast-method-call-chain-calls node)
  (cddr node))
