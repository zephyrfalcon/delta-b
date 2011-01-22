;; delta.scm

(push! *load-path* ".")
(use tokenizer)
(use parser)

(define *delta-version* "Delta/B 0.1")
(define *welcome-msg* (format "Welcome to Delta ~a." *delta-version*))
(define *prompt* ">> ")

(define (mainloop)
  (display *prompt*)
  (flush)
  (let ((line (read-line)))
    (if (eof-object? line)
        (print "Doegieeee!")
        (begin
          (print "Aha, you want to evaluate: " line)
          (mainloop)))))

(define (main args)
  (mainloop))

