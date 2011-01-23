;; interpreter.scm

(define-module interpreter
  (export-all))
(select-module interpreter)

(use gauche.record)
(use namespace)

(define-record-type interpreter #t #t
  toplevel-ns
  )

(define (new-interpreter)
  (let ((interp (make-interpreter (make-namespace #t))))
    (init-interpreter interp)
    interp))

(define (init-interpreter interp)
  interp)

