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

(define (assert x :optional (msg #f))
  (unless x (error (or msg "assertion failed"))))

(define (string-slice s idx1 :optional (idx2 #f))
  (let* ((sl (string-length s))
         (idx2 (or idx2 sl)) ;; defaults to length of string
         (corr-idx1 (if (negative? idx1) (+ sl idx1) idx1))
         (corr-idx2 (if (negative? idx2) (+ sl idx2) idx2)))
    #;(printf "(DEBUG) ~s, ~s~%" corr-idx1 corr-idx2)
    (if (>= corr-idx1 corr-idx2)
        ""
        (substring s
                   (min (max corr-idx1 0) sl)
                   (min (max corr-idx2 0) sl)))))
