;; tools.scm

(define-module tools
  (export-all))
(select-module tools)

(define (unique lst)
  (let ((hash (make-hash-table 'equal?)))
    (for-each (lambda (x) (hash-table-put! hash x #t))
              lst)
    (hash-table-keys hash)))
