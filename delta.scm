;; delta.scm
;; Delta REPL.

(push! *load-path* ".")
(use tokenizer)
(use parser)
(use interpreter)
(use tools)

(define *delta-version* "0.1.1")
(define *welcome-msg* (format "Welcome to Delta/B ~a." *delta-version*))
(define *prompt* ">> ")

(define (mainloop interp)
  (display *prompt*)
  (flush)
  (let ((line (read-line)))
    (if (eof-object? line)
        (print "Doegieeee!")
        (begin
          (printf "Aha, you want to evaluate: ~s~%" line)
          (delta-eval (string-append line "."))
          (mainloop interp)))))

(define (main args)
  (let ((interp (new-interpreter)))
    (print *welcome-msg*)
    (mainloop interp)))

