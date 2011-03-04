;; delta.scm
;; Delta REPL.

(push! *load-path* ".")
(use tokenizer)
(use parser)
(use tools)
(load "interpreter")
(load "delta-object")

(define *delta-version* "0.1.3")
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
          (delta-eval-string (string-append line ".") interp)
          (mainloop interp)))))

(define (main args)
  (let ((interp (new-interpreter)))
    (print *welcome-msg*)
    (mainloop interp)))

