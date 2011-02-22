;; interpreter.scm

(use srfi-1)
(use gauche.record)
(use namespace)
(use tokenizer)
(use parser)
(use pretty)
(use tools)

(load "proto-tools")
(load "builtin/object")
(load "builtin/string")
(load "builtin/integer")
(load "builtin/bmethod")

(define-record-type interpreter #t #t
  builtin-ns    ;; contains protos
  toplevel-ns   ;; user space
  )

(define (new-interpreter)
  (let* ((builtin-ns (make-namespace #f))
         (toplevel-ns (make-namespace builtin-ns)))
    (let ((interp (make-interpreter builtin-ns toplevel-ns)))
      (init-interpreter interp)
      interp)))

(define (init-interpreter interp)
  (add-protos interp)
  interp)

(define *protos*
  `(("Object"        ,make-object-proto)
    ("Integer"       ,make-integer-proto)
    ("String"        ,make-string-proto)
    ("BuiltinMethod" ,make-bmethod-proto)))

(define (add-protos interp)
  (let ((ns (interpreter-builtin-ns interp)))
    ;;(namespace-set! ns "Object" (make-object-proto interp))
    ;;(namespace-set! ns "Integer" (make-integer-proto interp))
    ;;(namespace-set! ns "String" (make-string-proto interp))
    ;;(namespace-set! ns "BuiltinMethod" (make-bmethod-proto interp))
    (for-each-pair
     (lambda (name proto-constructor)
       (namespace-set! ns name (proto-constructor interp)))
     *protos*)
    interp))

(define (find-builtin-proto interp name)
  (let ((ns (interpreter-builtin-ns interp)))
    (namespace-get ns name)))

;; Tokenize, parse and evaluate the Delta code in the given string.
(define (delta-eval-string s interp)
  (let* ((tokens (tokenize s))
         (_ (printf "[tokens] ~s~%" tokens))
         (stmts (match-program tokens)))
    (pretty-print stmts)
    (let ((result #f))
      (for-each
       (lambda (expr)
         (set! result
               (delta-eval expr (interpreter-toplevel-ns interp) interp)))
         stmts)
      (when result
        (printf "~s~%" (delta-object-repr result))))))
;; FIXME: printing should be done by the caller
;; and we should probably pass _all_ of the results, not just the last...

;; Evaluate the Delta expression EXPR (an AST object) in namespace NS.
(define (delta-eval expr ns interp)
  (cond ((ast-literal? expr)
         (case (second expr)
           ((integer) (new-integer-object interp (third expr)))
           ((float) ...)
           ((string) (new-string-object interp (third expr)))
           ((symbol) ...)
           ((identifier) ...)
           (else ...)))
        ((ast-block? expr)
         ...)
        ((ast-method-call-chain? expr)
         ...)
        (else (error "Unknown AST node type:" expr))))

