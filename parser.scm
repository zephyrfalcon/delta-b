;; parser.scm

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

4. A block consists of zero or more statements between "{" "}". (The
last statement inside the block does not need to be followed by
a ".".)

|#

