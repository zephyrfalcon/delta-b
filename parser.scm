;; parser.scm

(define-module parser
  (export-all))  ;; for now; be more selective later
(select-module parser)

#|

Grammar, sort of:

<literal> ::= integer | float | string | symbol | identifier
<expr> ::= <literal>
| <block>
| "(" <statement> ")"
<block> ::= "{" <statement>* "}"
<statement> ::= <method-call-chain> "."?
<method-call-chain> ::= <expr> <method-calls>*
<method-call> ::= <method-call-head> <args>*
<arg> ::= <expr>

A more informal description of the grammar:

1. We have a bunch of literals: integers (3, -17), floats (3.1415),
strings ("hello world"), symbols (#foo, #do-something),
identifiers (bar, is-true)

2. A method call chain consists of a head and zero or more method
calls, each of which contain of a method name and zero or more
arguments. A method name consists of an identifier plus a colon:

3 plus: 4
"hello world" length: println:
list: 1 2 3 println:

A statement is usually followed by a "." to indicate its end, but this
can be omitted if there is another token indicating the
end (specifically, a ")" or a "}").

3 plus: 4.
(5 minus: 2) println:.

The arguments of a method call are determined as follows: each
expression following the method name is considered an argument of that
method call, until another method call name is encountered, or a
terminator (".", ")", "}").

3. The head of a method call chain, and each of a method call's
arguments, can be an expression. An expression is a literal, a block
or a statement enclosed in parentheses.

4. A block consists of zero or more statements between "{" "}".
(The last statement inside the block does not need to be followed by a ".".)

[[NOTE: Some syntactic sugar will be added later, esp. for assignments.]]

|#

(use srfi-1)
(load "ast") ;; for now; turn into module later

(define *literals* '(integer float string identifier symbol))

(define (_match-zero-or-more matcher tokens)
  (let loop ((tokens-left tokens) (matched '()))
    (if (null? tokens-left)
        (values (reverse matched) tokens-left)
        (receive (match rest)
            (matcher tokens-left)
          (if match
              (loop rest (cons match matched))
              (values (reverse matched) tokens-left))))))

(define (_match-any matchers tokens)
  (if (null? matchers)
      (values #f #f)
      (receive (match rest)
          ((car matchers) tokens)
        (if match
            (values match rest)
            (_match-any (cdr matchers) tokens)))))

;;; --- matchers ---

;; The following matching procedures return two values; an AST node
;; (or list of nodes, if appropriate) and the list of tokens left
;; after matching, or #f #f if there was no match.

(define (match-program tokens)
  ;; returns a list of statements that make up the program.
  (let loop ((tokens-left tokens) (nodes-collected '()))
    (if (null? tokens-left)
        (reverse nodes-collected) ;; done
        (receive (match rest)
            (match-statement tokens-left)
          (if match
              (loop rest (cons match nodes-collected))
              (error "Invalid syntax: " tokens-left))))))

;; a statement is a method call chain followed by a terminator (dot,
;; rparen, rbrace). note that the method call chain may have zero
;; method calls, i.e. consists of a single value.
(define (match-statement tokens)
  (receive (expr rest)
      (match-method-call-chain tokens)
    (if expr
        (cond ((null? rest)
               (error "Unexpected end of tokens"))
              ((equal? (caar rest) 'dot)
               (values expr (cdr rest)))
              ((member (caar rest) '(rparen rbrace))
               (values expr rest))
              (else (error "Invalid syntax: " rest)))
        (values #f #f))))

(define (match-expression tokens)
  (_match-any (list match-literal
                    match-block
                    match-parenthesized-statement)
              tokens))

(define (match-literal tokens)
  (let ((token (car tokens)))
    (if (member (car token) *literals*)
        (values (make-ast-node 'literal token)
                (cdr tokens))
        (values #f #f))))

(define (match-block tokens)
  (if (and (not (null? tokens))
           (equal? (caar tokens) 'lbrace))
      ;; match zero or more statements...
      (receive (stmts rest)
          (_match-zero-or-more match-statement (cdr tokens))
        ;; ...then match the "}"
        (if (and (not (null? rest))
                 (equal? (caar rest) 'rbrace))
            (values (make-ast-block stmts) (cdr rest))
            (values #f #f)))
      (values #f #f)))

(define (match-method-call-chain tokens)
  (receive (head rest-1)
      (match-expression tokens)
    (if head
        ;; match zero or more method calls
        (receive (method-calls rest-2)
            (_match-zero-or-more match-method-call rest-1)
          (values (normalize-method-call-chain
                   (make-ast-method-call-chain head method-calls))
                  rest-2))
        (values #f #f))))

(define (match-method-call tokens)
  ;; match a single method call, i.e. the method name and zero or more
  ;; arguments.
  (if (and (not (null? tokens))
           (equal? (caar tokens) 'method-call-name))
      ;; match zero or more arguments (expressions).
      ;; does NOT match terminators, that is a job for match-statement.
      (let ((method-call-name (cadar tokens)))
        (let loop ((tokens-left (cdr tokens)) (args '()))
          (receive (match rest)
              (match-expression tokens-left)
            (if match
                (loop rest (cons match args))
                (values (make-ast-method-call method-call-name (reverse args))
                        tokens-left)))))
      (values #f #f)))

;; currently only matches a method call chain between ( ).
(define (match-parenthesized-statement tokens)
  (if (and (not (null? tokens))
           (equal? (caar tokens) 'lparen))
      (receive (mcc rest)
          (match-method-call-chain (cdr tokens))
        (if mcc
            (if (and (not (null? rest))
                     (equal? (caar rest) 'rparen))
                (values mcc (cdr rest))
                (values #f #f))
            (values #f #f)))
      (values #f #f)))

;; If a method call chain consists of only a head (and no actual
;; method calls on that head), replace it with just the head.
(define (normalize-method-call-chain mcc)
  (if (and (ast-method-call-chain? mcc)
           (equal? (cddr mcc) '()))
      (cadr mcc)
      mcc))
