;; tools.scm

(define-module tools
  (export-all))
(select-module tools)

(define (unique lst)
  (let ((hash (make-hash-table 'equal?)))
    (for-each (lambda (x) (hash-table-put! hash x #t))
              lst)
    (hash-table-keys hash)))

(define (printf str . args)
  (display (apply format (cons str args))))

(define (for-each-pair f lst)
  (for-each
   (lambda (x)
     (f (car x) (cadr x)))
   lst))
