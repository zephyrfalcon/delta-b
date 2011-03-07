;; delta.scm
;; Delta REPL.

(push! *load-path* ".")
(use tokenizer)
(use parser)
(use tools)
(load "interpreter")
(load "delta-object")

(define *delta-version* "0.1.5")
(define *welcome-msg* (format "Welcome to Delta/B ~a." *delta-version*))
(define *prompt* ">> ")
(define *debug* #t)

(define (mainloop interp)
  (display *prompt*)
  (flush)
  (let ((line (read-line)))
    (if (eof-object? line)
        (print "Doegieeee!")
        (begin
          (when *debug*
            (printf "Aha, you want to evaluate: ~s~%" line))
          (let ((result (delta-eval-string (string-append line ".") interp)))
            (when result
              (let* ((ns (interpreter-toplevel-ns interp))
                     (repr (get-delta-object-repr result ns interp)))
                (printf "~a~%" (delta-object-data repr)))))
          (mainloop interp)))))

(define (main args)
  (let ((interp (new-interpreter)))
    (print *welcome-msg*)
    (mainloop interp)))

