;; interpreter.scm

(use gauche.record)
(use namespace)
(use tokenizer)
(use parser)
(use pretty)
(use tools)

(load "builtin/object")
(load "builtin/string")
(load "builtin/integer")

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

(define (add-protos interp)
  (let ((ns (interpreter-builtin-ns interp)))
    (namespace-set! ns "Object" (make-object-proto interp))
    (namespace-set! ns "Integer" (make-integer-proto interp))
    (namespace-set! ns "String" (make-string-proto interp))
    interp))

(define (find-builtin-proto interp name)
  (let ((ns (interpreter-builtin-ns interp)))
    (namespace-get ns name)))

(define (delta-eval s)
  (let* ((tokens (tokenize s))
         (_ (printf "[tokens] ~s~%" tokens))
         (stmts (match-program tokens)))
    (for-each pretty-print stmts)))
