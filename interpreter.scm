;; interpreter.scm

(define-module interpreter
  (export-all))
(select-module interpreter)

(use gauche.record)
(use namespace)
(use tokenizer)
(use parser)
(use pretty)
(use tools)

(define-record-type interpreter #t #t
  toplevel-ns
  )

(define (new-interpreter)
  (let ((interp (make-interpreter (make-namespace #t))))
    (init-interpreter interp)
    interp))

(define (init-interpreter interp)
  interp)

(define (delta-eval s)
  (let* ((tokens (tokenize s))
         (_ (printf "[tokens] ~s~%" tokens))
         (stmts (match-program tokens)))
    (for-each pretty-print stmts)))
