;; test-auto.scm

;; TODO:
;; - proc that takes a "partition", evaluates the code, checks the (printed)
;;   result against the answer given
;; - stuff that runs all these tests using the gauche.test mechanism

(push! *load-path* ".")
(use file.util)
(use srfi-13)
(use tools)

(define *test-files*
  (directory-list "tests" :add-path? #t
                  :filter (^f (string-suffix? ".txt" f))))

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
  
  (let* ((data (read-all-from-file filename))
         (lines (string-split data #\newline))
         (partitions (partition-lines lines)))
    (printf "~s~%" partitions)))

(for-each test-auto-file *test-files*)
