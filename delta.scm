;; delta.scm

(push! *load-path* ".")
(use tokenizer)
(use parser)
(use interpreter)

(define *delta-version* "Delta/B 0.1")
(define *welcome-msg* (format "Welcome to Delta ~a." *delta-version*))
(define *prompt* ">> ")

(define (mainloop interp)
  (display *prompt*)
  (flush)
  (let ((line (read-line)))
    (if (eof-object? line)
        (print "Doegieeee!")
        (begin
          (print "Aha, you want to evaluate: " line)
          (mainloop interp)))))

(define (main args)
  (let ((interp (new-interpreter)))
    (print *welcome-msg*)
    (mainloop interp)))

