;; test-auto.scm

;; TODO:
;; - proc that takes a "partition", evaluates the code, checks the (printed)
;;   result against the answer given
;; - stuff that runs all these tests using the gauche.test mechanism

(push! *load-path* ".")
(use file.util)
(use gauche.test)
(use srfi-13)
(use tools)
(load "interpreter")
(load "delta-object")

(define *test-files*
  (directory-list "tests" :add-path? #t
                  :filter (^f (string-suffix? ".txt" f))))

(define *debug* #f)

(define (test-auto-file filename)

  ;; Split a list of lines into "partitions". The end of a partition
  ;; is indicated by a line starting with "=>" (the partition still
  ;; includes that line).
  (define (partition-lines lines)
    (let loop ((lines lines) (curr-par '()) (partitions '()))
      (cond ((null? lines)
             (reverse partitions))
            ((string-prefix? "=>" (car lines))
             (loop (cdr lines) '()
                   (cons (reverse (cons (car lines) curr-par))
                         partitions)))
            (else
             (loop (cdr lines) (cons (car lines) curr-par) partitions)))))

  (define (test-partition part)
    (receive (butlast last)
        (split-at-last part)
      (let ((code (maybe-append-dot (string-join butlast "\n")))
            (result-repr (string-trim (string-slice (car last) 2))))
        #;(printf "code: ~s~%" code)
        (let ((interp (new-interpreter)))
          (let* ((result (delta-eval-string code interp))
                 (ns (interpreter-toplevel-ns interp))
                 (dx-repr (get-delta-object-repr result ns interp))
                 (real-repr (delta-object-data dx-repr)))
            (test* (format "~s" code) result-repr real-repr))))))
  
  (let* ((data (read-all-from-file filename))
         (lines (string-split data #\newline))
         (partitions (partition-lines lines)))
    (for-each (lambda (part)
                #;(printf "~s~%" part)
                (test-partition part))
              partitions)))

(for-each test-auto-file *test-files*)
